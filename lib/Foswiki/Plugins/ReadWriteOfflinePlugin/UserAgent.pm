# WRC466130775A3661534B79534655
# Copyright (C) 2006 C-dot Consultants
# All rights reserved.
package Foswiki::Plugins::ReadWriteOfflinePlugin::UserAgent;

use strict;

use LWP;

@Foswiki::Plugins::ReadWriteOfflinePlugin::UserAgent::ISA = qw(LWP::UserAgent);

sub new {
    my ($class, $syncher) = @_;
    # Allocate a cookie jar for this agent
    my $jar = "$syncher->{work_area}/cookie_jar$$";
    my $this = $class->SUPER::new(keep_alive => 1);
    require HTTP::Cookies;
    $this->cookie_jar(new HTTP::Cookies(
        file => $jar,
        autosave => 1,
        ignore_discard => 1));
    $this->{syncher} = $syncher;
    $this->{jar} = $jar;
    $this->agent('ReadWriteOfflinePlugin/$Rev$');
    return $this;
}

sub DESTROY {
    my $this = shift;
    # Whack the cookie jar
    unlink($this->{jar});
}

sub get_basic_credentials {
    my($this, $realm, $uri) = @_;
    my $syncher = $this->{syncher};
    return ($syncher->{username}, $syncher->{password});
}

1;
