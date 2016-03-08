# ShortestPaths example, from figure 1 of "Logic and lattices for
# distributed programming", Conway et. al, UCB tech report, 2012
#
# Modified slightly:
#  - added "bootstrap" section to give the algorithm some data
#  - added "stdio <~ ..." to monitor results
#  - added boilerplate to kickstart the program
#  - repurposed next_hop field to track seen-set to avoid nontermination

require 'rubygems'
require 'bud'
require 'set'

class ShortestPaths
  include Bud

  state do
    table :link, [:from, :to] => [:cost]
    scratch :path, [:from, :to, :seen, :cost]
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
    path <= link {|l| [l.from, l.to, Set.new.add(l.from).add(l.to), l.cost]}
    path <= (link*path).pairs(:to => :from) do |l,p|
      [l.from, p.to, p.seen.clone.add(l.from), l.cost + p.cost] if not p.seen.include?(l.from)
    end
    min_cost <= path.group([:from, :to], min(:cost))

    stdio <~ path { |p| ["path" + p.to_s] }
    stdio <~ min_cost { |m| ["min_cost" + m.to_s] }
  end
end

program = ShortestPaths.new()
program.run_fg
