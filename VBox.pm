package Modem::VBox;

use strict 'subs';
use Carp;

require Exporter;
use Event qw(unloop one_event time unloop_all);
use POSIX ':termios_h';
use Fcntl;
use Event::Watcher qw(R W);

BEGIN { $^W=0 } # I'm fed up with bogus and unnecessary warnings nobody can turn off.

@ISA = qw(Exporter);

@_consts = qw();
@_funcs = qw();

@EXPORT = @_consts;
@EXPORT_OK = @_funcs;
%EXPORT_TAGS = (all => [@_consts,@_funcs], constants => \@_consts);
$VERSION = '0.02';

# if debug is used, STDIN will be used for events and $play will be used to play messages
$debug = 0;

# hardcoded constants
$HZ=8000;
$FRAG=1024; # ~~ 8 Hz.

$ETX="\003";
$DLE="\020";
$DC4="\024";

# bit flags for state var
sub VCON	(){ 1 }
sub VTX		(){ 2 }
sub VRX		(){ 4 }

sub slog {
   my $self=shift;
   my $level=shift;
   print STDERR $self->{line},": ",@_,"\n" if $level <= $debug;
}

#  port => /dev/ttyI0
sub new {
   my $class = shift;
   my(%attr)=@_;

   croak "line must be specified" unless $attr{line};

   eval { $attr{speed}	||= &B115200 };
   eval { $attr{speed}	||= &B57600  };
   $attr{speed}		||= B38400;

   $attr{dropdtrtime}	||= 0.25; # dtr timeout
   $attr{modeminit}	||= "ATZ";
   $attr{ringto}	||= 5; # ring-timeout
   $attr{rings}		||= 3; # number of rings

   my $self = bless \%attr,$class;

   $self->slog(3,"opening line");

   $self->{fh}=local *FH;
   sysopen $self->{fh},$attr{line},O_RDWR|O_NONBLOCK
      or croak "unable to open device $attr{line} for r/w";
   $self->{fileno}=fileno $self->{fh};

   $self->{tio} = new POSIX::Termios;
   $self->{tio}->getattr($self->{fileno});

   $self->{inwatcher}=Event->io(
      e_poll => R,
      e_fd => $self->{fileno},
      e_desc => "Modem input for $self->{line}",
      e_cb => sub {
         my $ri = \($self->{rawinput});
         if (sysread($self->{fh},$$ri,8192,length($$ri)) == 0) {
            $self->slog(1,"short read, probably remote hangup");
            $self->{state} &= ~(VCON|VRX|VTX);
            $self->hangup;
         } else {
            if ($self->{state} & VRX) {
               my $changed;
               # must use a two-step process
               $$ri =~ s/^((?:[^$DLE]+|$DLE[^$ETX$DC4])*)//o;
               my $data=$1;
               $data =~ s{$DLE(.)}{
                  if ($1 eq $DLE) {
                     $DLE;
                  } else {
                     $self->{break}.=$1;
                     print "got dle seq ($1)\n";#d#
                     $changed=1;
                     "";
                  }
               }ego;
               if ($$ri =~ s/^$DLE$ETX[\r\n]*VCON[^\r\n]*[\r\n]+//o) {
                  $self->slog(3,"=> ETX, EO VRX");
                  $self->{state} &= ~VRX;
               }
               $self->check_break if $changed;
            }
            unless ($self->{state} & VRX) {
               while ($$ri =~ s/^([^\r\n]*)[\r\n]+//) {
                  local $_ = $1;
                  if (/^CALLER NUMBER:\s+(\d+)$/) {
                     $self->{_callerid}=$1;
                     $self->slog(3,"incoming call has callerid $1");
                  } elsif (/^RING\b/) {
                     my $cid = delete $self->{_callerid} || "0";
                     my $oci = $self->{callerid};
                     $self->{callerid}=$cid;
                     if (defined $oci) {
                        if ($oci ne $cid) {
                           $self->rung;
                           $self->{ring}=0;
                        }
                     } else {
                        $self->{ring}=0;
                     }
                     $self->{ringtowatcher}{e_at}=time+$self->{ringto};
                     $self->{ringtowatcher}->resume;
                     $self->ring(++$self->{ring});
                  } elsif (/^RUNG\b/) {
                     $self->rung;
                  } elsif (/\S/) {
                     push(@{$self->{modemresponse}},$_);
                  }
               }
            }
         }
      }
   );
   $self->{outwatcher}=Event->io(
      e_poll => 0,
      e_fd => $self->{fileno},
      e_desc => "Modem sound output for $self->{line}",
      e_cb => sub {
         my $l;
         my $o = \($self->{rawoutput});
         if(!$$o) {
            my $q = $self->{play_queue};
            if (@$q) {
               if (ref \($q->[0]) eq "GLOB") {
                  my $n;
                  $l=sysread $q->[0],$n,$FRAG;
                  $n=~s/$DLE/$DLE$DLE/go;
                  $$o.=$n;
                  shift(@$q) if $l<=0;
               } else {
                  $$o=${shift(@$q)};
               }
            } else {
               $self->{outwatcher}{e_poll}=0;
            }
         }
         if ($$o) {
            $l = syswrite($self->{fh},$$o);
            substr($$o,0,$l)="" if $l>0;
         }
      }
   );
   $self->{ringtowatcher}=Event->timer(
      e_at => 1, # ignored, but must be set
      e_desc => 'RING timeout detector',
      e_cb => sub {
         $self->rung;
         $self->slog(1,"ring timeout, aborted connection");
      }
   );
   $self->{ringtowatcher}->suspend;

   $self->reset;
   $self->initialize;
   $self;
}

sub DESTROY {
   my $self=shift;
   $self->{tio}->setispeed(B0); $self->{tio}->setospeed(B0); $self->sane;
   close $self->{fh} or croak "error during modem-close: $!";
}

sub flush {
   my $self=shift;
   undef $self->{rawinput};
   tcflush($self->{fileno},TCIOFLUSH);
}

sub sane {
   my $self=shift;
   $self->{tio}->setiflag(BRKINT|IGNPAR|IXON);
   $self->{tio}->setoflag(OPOST);
   $self->{tio}->setcflag($self->{tio}->getcflag
                  &~(CSIZE|CSTOPB|PARENB|PARODD|CLOCAL)
                  | (CS8|CREAD|HUPCL));
   $self->{tio}->setlflag(ECHOK|ECHOE|ECHO|ISIG|ICANON);
   $self->{tio}->setattr($self->{fileno});
   $self->{tio}->setcc(VMIN,1);
   $self->{tio}->setcc(VTIME,0);
}

sub raw {
   my $self=shift;
   $self->{tio}->setiflag($self->{tio}->getiflag & (IXON|IXOFF));
   $self->{tio}->setoflag(0);
   $self->{tio}->setcflag(0);
   $self->{tio}->setlflag(0);
   $self->{tio}->setcc(VMIN,1);
   $self->{tio}->setcc(VTIME,0);
   $self->{tio}->setattr($self->{fileno});
}

sub reset {
   my $self=shift;

   $self->sane;

   my $i=$self->{tio}->getispeed; my $o=$self->{tio}->getospeed;
   $self->{tio}->setispeed(B0); $self->{tio}->setospeed(B0);

   $self->{tio}->setattr($self->{fileno});

   my $w = Event->timer(e_after => $self->{dropdtrtime},
                        e_cb => sub { unloop },
                        e_desc => 'Modem DTR drop timeout');
   $self->{tio}->setispeed($i); $self->{tio}->setospeed($o);

   $self->slog(3,"waiting for reset");
   $self->loop;
   $self->slog(3,"line reset");

   $self->{tio}->setattr($self->{fileno});

   $self->raw;
   $self->flush;
   my $buf; 1 while (sysread ($self->{fh},$buf,1024) > 0);

   $self->command("AT")=~/^OK/ or croak "modem returned $self->{resp} to AT";
   $self->command($self->{modeminit})=~/^OK/ or croak "modem returned $self->{resp} to modem init string";
   $self->command("AT+VLS=2")=~/^OK/ or croak "modem returned $self->{resp} to AT+VLS=2";
   $self->command("AT+VSM=6")=~/^OK/ or croak "modem returned $self->{resp} to AT+VSM=6";
}

# read a line
sub modemline {
   my $self=shift;
   one_event while(!@{$self->{modemresponse}});
   shift(@{$self->{modemresponse}});
}

sub command {
   my $self = shift;
   my $cmd = shift;
   fcntl $self->{fh},F_SETFL,0;
   syswrite $self->{fh},"$cmd\r";
   fcntl $self->{fh},F_SETFL,O_NONBLOCK;
   $self->{resp} = $self->modemline;
   $self->{resp} = $self->modemline if $self->{resp} eq $cmd;
   $self->slog(2,"COMMAND($cmd) => ",$self->{resp});
   $self->{resp};
}

sub initialize {
   my $self=shift;
   delete @{$self}{qw(play_queue state context break callerid
                      rawinput rawoutput modemresponse)};
   $self->{ringtowatcher}->suspend;
   $self->{tio}->setispeed($self->{speed});
   $self->{tio}->setospeed($self->{speed});
}

sub abort {
   my $self=shift;
   $self->reset;
   $self->initialize;
   $self->slog(1,"modem is now in listening state");
}

sub ring {
   my $self=shift;
   my $count=shift;
   $self->slog(1,"the telephone rings (#$count), hurry! (callerid $self->{callerid})");
   $self->accept if $count >= $self->{rings};
}

sub rung {
   my $self=shift;
   $self->slog(1,"caller ($self->{callerid}) hung up before answering");
   delete $self->{callerid};
}

sub loop {
   local $Event::DIED = sub {
      print STDERR $_[1];
      unloop_all;
   };
   Event::loop;
}

sub accept {
   my $self=shift;
   # DLE etc. handling
   $self->{ringtowatcher}->suspend;
   if ($self->command("ATA") =~ /^VCON/) {
      $self->slog(2,"call accepted (callerid $self->{callerid})");
      if ($self->command("AT+VTX+VRX") =~ /^CONNECT/) {
         $self->raw;
         $self->{state}|=VCON|VTX|VRX;
         $self->{connect_cb}->($self);
      } else {
         $self->abort;
         $self->slog(1,"modem did not respond with CONNECT to AT+VTX+VRX command");
      }
   } else {
      $self->slog(1,"modem did not respond with VCON to my ATA");
      $self->rung;
   }
}

sub check_break {
   my $self=shift;
   while(my($k,$v)=each(%{$self->{context}})) {
      if ($self->{break}=~/$k/) {
         ref $v eq "CODE"
         ? $v->($self,$self->{break})
         : $self->event($v);
      }
   }
}

sub hangup {
   my $self=shift;
   $self->abort;
}

sub connected {
   $_[0]->{state} & VCON;
}

sub event {
   my $self=shift;
   $self->{event}=shift;
}

sub play_flush {
   my $self=shift;
   tcflush($self->{fileno},TCOFLUSH);
   @{$self->{play_queue}}=();
   delete $self->{rawoutput};
   print "flushing\n";
   one_event;
   print "flushed\n";
}

sub play_file {
   my $self=shift;
   my $path=shift;
   my($fh)=local *FH;
   open $fh,"<$path" or croak "unable to open ulaw file '$path' for playing";
   $self->play_object($fh);
}

sub play_data {
   my $self=shift;
   my $data=shift;
   $data=~s/$DLE/$DLE$DLE/go;
   $self->play_object(\$data);
}

sub play_object {
   my $self=shift;
   my $obj=shift;
   $self->{state} & VCON or croak "can't start plaing when not connected";
   push(@{$self->{play_queue}},$obj);
   $self->{outwatcher}->{e_poll} = W;
}

sub play_pause {
   my $self=shift;
   my $len = int($HZ*$_[0]+0.999);
   my $k8  = "\xFE" x $FRAG;
   while ($len>length($k8)) {
      $self->play_object(\$k8);
      $len-=length($k8);
   }
   $self->play_object(\("\xFE" x $len));
}

sub play_count {
   scalar @{$_[0]->{play_queue}};
}

# wait until one file has been played
sub play_wait {
   my $self=shift;
   my $pcount = $self->play_count;
   delete $self->{event};
   if ($pcount) {
      do { 
         one_event;
      } while $pcount == $self->play_count
           && !exists $self->{event};
   }
   delete $self->{event};
}

sub context {
   my $self=shift;
   bless [$self,%{$self->{context}}],"Modem::VBox::context";
}

package Modem::VBox::context;

sub set {
   my $self=shift;
   %{$self->[0]{context}} = @_;
   $self;
}

*clr = \&set;

sub add {
   my $self=shift;
   while(@_) {
      $self->[0]{context}{$_[0]} = $_[1];
      shift; shift;
   }
   $self;
}

sub del {
   my $self=shift;
   for(@_) {
      delete $self->[0]{context}{$_};
   }
   $self;
}

sub DESTROY {
   my $self=shift;
   my($vbox,$ctx)=@$self;
   $vbox->{context}=$ctx;
}

1;
__END__

=head1 NAME

Modem::VBox - Perl module for creation of voiceboxes.

=head1 SYNOPSIS

  use Modem::VBox;

=head1 DESCRIPTION

Oh well ;) Not written yet! An example script is included in the distro, though.

=head1 AUTHOR

Marc Lehmann <pcg@goof.com>.

=head1 SEE ALSO

perl(1), L<Modem::Vgetty> a similar but uglier interface.

=cut
