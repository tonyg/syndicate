# ShortestPaths example, from figure 1 of "Logic and lattices for
# distributed programming", Conway et. al, UCB tech report, 2012
#
# Modified slightly:
#  - added "bootstrap" section to give the algorithm some data
#  - added "stdio <~ ..." to monitor results
#  - added boilerplate to kickstart the program
#
# Problem: doesn't terminate or yield output, because cycles exist in
# the input.

require 'rubygems'
require 'bud'

class ShortestPaths
  include Bud

  state do
    table :link, [:from, :to] => [:cost]
    scratch :path, [:from, :to, :next_hop, :cost]
    scratch :min_cost, [:from, :to] => [:cost]
  end

  bootstrap do
    link <= [[1, 3, -2],
             [2, 1, 4],
             [2, 3, 3],
             [3, 4, 2],
             [4, 2, -1]]
  end

  bloom do
    path <= link {|l| [l.from, l.to, l.to, l.cost]}
    path <= (link*path).pairs(:to => :from) do |l,p|
      [l.from, p.to, l.to, l.cost + p.cost]
    end
    min_cost <= path.group([:from, :to], min(:cost))

    stdio <~ path { |p| ["path" + p.to_s] }
    stdio <~ min_cost { |m| ["min_cost" + m.to_s] }
  end
end

program = ShortestPaths.new()
program.run_fg
