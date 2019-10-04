#!/bin/sh

for ruby in \
    $HOME/.rubies/ruby-2.6.5 \
    $HOME/.rubies/jruby-9.2.8.0 \
    $HOME/.rubies/truffleruby-19.2.0.1
do
    echo $ruby
    ../xtime.rb $ruby/bin/ruby matmul.rb 1500
done
