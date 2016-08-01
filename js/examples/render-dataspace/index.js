"use strict";

var dataspace_blobs = [
  [1470020507797.488,[3,3],
   [[[1,"standalone-assertions"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[10.0,[[],[],[[{"seal":{"misc":"((scale 4000 2250) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]]],[],[]]],
    [[5,"(ground-block #(25 125) #(100 50) purple)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[0.0,[[],[],[[{"seal":{"misc":"((translate 25 125) (scale 100 50) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[4,":game-piece-configuration",[[],[],[["ground-block272",[[[2,"V",[[],[],[[25.0,[[],[],[[125.0,[[[2,"V",[[],[],[[100.0,[[],[],[[50.0,[[],[],[[{"misc":"#<set: solid>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[9,"(goal-piece 570 150)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[-1.0,[[],[],[[{"seal":{"misc":"((translate 2179/4 408/5) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[1,":observe",[[[3,":touching",[[],[],[["player",[[],[],[["goal271",[[],[true],[]]]]]]]]]],[],[]]],[4,":game-piece-configuration",[[],[],[["goal271",[[[2,"V",[[],[],[[561.5833333333334,[[],[],[[115.8,[[[2,"V",[[],[],[[16.833333333333332,[[],[],[[34.2,[[],[],[[{"misc":"#<set: touchable>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[14,"(ground-block #(500 600) #(150 50) orange)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[0.0,[[],[],[[{"seal":{"misc":"((translate 500 600) (scale 150 50) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[4,":game-piece-configuration",[[],[],[["ground-block280",[[[2,"V",[[],[],[[500.0,[[],[],[[600.0,[[[2,"V",[[],[],[[150.0,[[],[],[[50.0,[[],[],[[{"misc":"#<set: solid>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[3,"physics-engine"],[[[1,":observe",[[[1,":inbound",[[[1,":inbound",[[[1,":inbound",[[[4,":frame-event",[[],[[],[[],[[],[true],[]],[]],[]],[]]]],[],[]]]],[],[]]]],[],[]]],[1,":jump-request",[[],[true],[]]],[2,":impulse",[[],[[],[true],[]],[]]],[4,":game-piece-configuration",[[],[[],[[],[[],[true],[]],[]],[]],[]]]],[],[]]],[3,":position",[[],[],[["ground-block272",[[[2,"V",[[],[],[[25.0,[[],[],[[125.0,[[[2,"V",[[],[],[[100.0,[[],[],[[50.0,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["ground-block273",[[[2,"V",[[],[],[[50.0,[[],[],[[300.0,[[[2,"V",[[],[],[[500.0,[[],[],[[50.0,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["enemy274",[[[2,"V",[[],[],[[369.3477636718751,[[],[],[[271.5,[[[2,"V",[[],[],[[45.45,[[],[],[[28.5,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["enemy275",[[[2,"V",[[],[],[[108.107396484375,[[],[],[[271.5,[[[2,"V",[[],[],[[45.45,[[],[],[[28.5,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["goal271",[[[2,"V",[[],[],[[561.5833333333334,[[],[],[[115.8,[[[2,"V",[[],[],[[16.833333333333332,[[],[],[[34.2,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["ground-block276",[[[2,"V",[[],[],[[850.0,[[],[],[[300.0,[[[2,"V",[[],[],[[50.0,[[],[],[[50.0,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["ground-block277",[[[2,"V",[[],[],[[925.0,[[],[],[[400.0,[[[2,"V",[[],[],[[50.0,[[],[],[[50.0,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["ground-block278",[[[2,"V",[[],[],[[975.0,[[],[],[[500.0,[[[2,"V",[[],[],[[50.0,[[],[],[[50.0,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["ground-block279",[[[2,"V",[[],[],[[975.0,[[],[],[[600.0,[[[2,"V",[[],[],[[50.0,[[],[],[[50.0,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["ground-block280",[[[2,"V",[[],[],[[500.0,[[],[],[[600.0,[[[2,"V",[[],[],[[150.0,[[],[],[[50.0,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]],["player",[[[2,"V",[[],[],[[41.583333333333336,[[],[],[[99.10591351730179,[[[2,"V",[[],[],[[16.833333333333332,[[],[],[[25.65,[true]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[7,"(enemy 100 300 right)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[-1.0,[[],[],[[{"seal":{"misc":"((translate 366.8227636718751 915/4) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[1,":observe",[[[1,":level-size",[[],[true],[]]],[3,":position",[[],[],[["enemy274",[[],[[],[true],[]],[]]]]]],[3,":touching",[[],[],[["player",[[],[],[["enemy274",[[],[true],[]]]]]]]]]],[],[]]],[2,":impulse",[[],[],[["enemy274",[[[2,"V",[[],[],[[0.2,[[],[],[[0.0,[true]]]]]]]]],[],[]]]]]],[4,":game-piece-configuration",[[],[],[["enemy274",[[[2,"V",[[],[],[[77.275,[[],[],[[271.5,[[[2,"V",[[],[],[[45.45,[[],[],[[28.5,[[],[],[[{"misc":"#<set: massive mobile touchable>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[11,"(ground-block #(925 400) #(50 50) purple)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[0.0,[[],[],[[{"seal":{"misc":"((translate 925 400) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[4,":game-piece-configuration",[[],[],[["ground-block277",[[[2,"V",[[],[],[[925.0,[[],[],[[400.0,[[[2,"V",[[],[],[[50.0,[[],[],[[50.0,[[],[],[[{"misc":"#<set: solid>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[13,"(ground-block #(975 600) #(50 50) purple)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[0.0,[[],[],[[{"seal":{"misc":"((translate 975 600) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[4,":game-piece-configuration",[[],[],[["ground-block279",[[[2,"V",[[],[],[[975.0,[[],[],[[600.0,[[[2,"V",[[],[],[[50.0,[[],[],[[50.0,[[],[],[[{"misc":"#<set: solid>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[2,"display-controller"],[[[1,":outbound",[[[1,":outbound",[[[1,":scroll-offset",[[[2,"V",[[],[],[[0.0,[[],[],[[0.0,[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[1,":observe",[[[1,":inbound",[[[1,":inbound",[[[1,":inbound",[[[2,":window",[[],[[],[true],[]],[]]]],[],[]]]],[],[]]]],[],[]]],[3,":position",[[],[],[["player",[[[2,"V",[[],[[],[[],[true],[]],[]],[]]]],[],[]]]]]]],[],[]]],[1,":level-size",[[[2,"V",[[],[],[[4000.0,[[],[],[[2000.0,[true]]]]]]]]],[],[]]]],[],[]]],
    [[6,"(ground-block #(50 300) #(500 50) purple)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[0.0,[[],[],[[{"seal":{"misc":"((translate 50 300) (scale 500 50) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[4,":game-piece-configuration",[[],[],[["ground-block273",[[[2,"V",[[],[],[[50.0,[[],[],[[300.0,[[[2,"V",[[],[],[[500.0,[[],[],[[50.0,[[],[],[[{"misc":"#<set: solid>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[10,"(ground-block #(850 300) #(50 50) purple)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[0.0,[[],[],[[{"seal":{"misc":"((translate 850 300) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[4,":game-piece-configuration",[[],[],[["ground-block276",[[[2,"V",[[],[],[[850.0,[[],[],[[300.0,[[[2,"V",[[],[],[[50.0,[[],[],[[50.0,[[],[],[[{"misc":"#<set: solid>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [["meta",false],[[[1,":observe",[[[1,":outbound",[[],[true],[]]],[1,":observe",[[[1,":inbound",[[],[true],[]]]],[],[]]]],[],[]]],[1,":inbound",[[[1,":inbound",[[[1,":inbound",[[[2,":window",[[],[],[[600.0,[[],[],[[400.0,[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]]],[],[]]],
    [[0,"#f"],[[[1,":outbound",[[[0,":level-running",[true]]],[],[]]],[1,":observe",[[[1,":inbound",[[[0,":level-completed",[true]]],[],[]]],[4,":game-piece-configuration",[[],[],[["player",[[],[[],[[],[true],[]],[]],[]]]]]]],[],[]]]],[],[]]],
    [[4,"player-avatar"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[0.0,[[],[],[[{"seal":{"misc":"((translate 99/4 55.287163517301785) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[1,":observe",[[[1,":inbound",[[[1,":inbound",[[[1,":key-pressed",[[],[true],[]]]],[],[]]]],[],[]]],[2,":damage",[[],[],[["player",[[],[true],[]]]]]],[3,":position",[[],[],[["player",[[],[[],[true],[]],[]]]]]]],[],[]]],[2,":impulse",[[],[],[["player",[[[2,"V",[[],[],[[0.0,[[],[],[[0.0,[true]]]]]]]]],[],[]]]]]],[2,":health",[[],[],[["player",[[],[],[[1.0,[true]]]]]]]],[4,":game-piece-configuration",[[],[],[["player",[[[2,"V",[[],[],[[41.583333333333336,[[],[],[[24.35,[[[2,"V",[[],[],[[16.833333333333332,[[],[],[[25.65,[[],[],[[{"misc":"#<set: massive mobile player>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[8,"(enemy 300 300 left)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[-1.0,[[],[],[[{"seal":{"misc":"((translate 105.582396484375 915/4) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[1,":observe",[[[1,":level-size",[[],[true],[]]],[3,":position",[[],[],[["enemy275",[[],[[],[true],[]],[]]]]]],[3,":touching",[[],[],[["player",[[],[],[["enemy275",[[],[true],[]]]]]]]]]],[],[]]],[2,":impulse",[[],[],[["enemy275",[[[2,"V",[[],[],[[0.2,[[],[],[[0.0,[true]]]]]]]]],[],[]]]]]],[4,":game-piece-configuration",[[],[],[["enemy275",[[[2,"V",[[],[],[[277.275,[[],[],[[271.5,[[[2,"V",[[],[],[[45.45,[[],[],[[28.5,[[],[],[[{"misc":"#<set: massive mobile touchable>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]],
    [[12,"(ground-block #(975 500) #(50 50) purple)"],[[[1,":outbound",[[[1,":outbound",[[[1,":outbound",[[[2,":sprite",[[],[],[[0.0,[[],[],[[{"seal":{"misc":"((translate 975 500) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]],[4,":game-piece-configuration",[[],[],[["ground-block278",[[[2,"V",[[],[],[[975.0,[[],[],[[500.0,[[[2,"V",[[],[],[[50.0,[[],[],[[50.0,[[],[],[[{"misc":"#<set: solid>"},[true]]]]]]]]]]]],[],[]]]]]]]]]],[],[]]]]]]],[],[]]]]],

  [1470020507810.975,[3],
   [[[1,"score-keeper"],[[[1,":outbound",[[[3,":on-screen-display",[[],[],[[-150.0,[[],[],[[10.0,[[],[],[[{"seal":{"misc":"#(struct:object:image% ...)"}},[true]]]]]]]]]]]],[],[]]],[1,":observe",[[[1,":add-to-score",[[],[true],[]]]],[],[]]],[1,":current-score",[[],[],[[0.0,[true]]]]]],[],[]]],
    [[3,"#f"],[[[0,":level-running",[true]],[1,":outbound",[[[1,":scroll-offset",[[[2,"V",[[],[],[[0.0,[[],[],[[0.0,[true]]]]]]]]],[],[]]],[1,":outbound",[[[2,":sprite",[[],[],[[-1.0,[[],[],[[{"seal":{"misc":"((translate 2179/4 408/5) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]],[{"seal":{"misc":"((translate 366.8227636718751 915/4) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]],[{"seal":{"misc":"((translate 105.582396484375 915/4) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]]]]],[0.0,[[],[],[[{"seal":{"misc":"((translate 25 125) (scale 100 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 50 300) (scale 500 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 850 300) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 925 400) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 975 500) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 975 600) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 500 600) (scale 150 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 99/4 55.287163517301785) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]]]]],[10.0,[[],[],[[{"seal":{"misc":"((scale 4000 2250) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]]],[],[]]],[1,":observe",[[[0,":level-completed",[true]],[1,":inbound",[[[1,":key-pressed",[[],[true],[]]],[1,":inbound",[[[2,":window",[[],[[],[true],[]],[]]],[4,":frame-event",[[],[[],[[],[[],[true],[]],[]],[]],[]]]],[],[]]]],[],[]]]],[],[]]]],[],[]]],
    [[2,"level-spawner"],[[[1,":observe",[[[0,":level-completed",[true]],[0,":level-running",[true]]],[],[]]]],[],[]]],
    [["meta",false],[[[1,":observe",[[[1,":outbound",[[],[true],[]]],[1,":observe",[[[1,":inbound",[[],[true],[]]]],[],[]]]],[],[]]],[1,":inbound",[[[1,":inbound",[[[2,":window",[[],[],[[600.0,[[],[],[[400.0,[true]]]]]]]]],[],[]]]],[],[]]]],[],[]]]]],

  [1470020507811.753,[],
   [[[1,"#f"],[[[1,":observe",[[[1,":inbound",[[[3,":key-event",[[],[[],[[],[true],[]],[]],[]]]],[],[]]]],[],[]]]],[],[]]],
    [[3,"#f"],[[[1,":scroll-offset",[[[2,"V",[[],[],[[0.0,[[],[],[[0.0,[true]]]]]]]]],[],[]]],[1,":outbound",[[[2,":sprite",[[],[],[[-1.0,[[],[],[[{"seal":{"misc":"((translate 2179/4 408/5) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]],[{"seal":{"misc":"((translate 101.06381640625 915/4) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]],[{"seal":{"misc":"((translate 366.8227636718751 915/4) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]]]]],[0.0,[[],[],[[{"seal":{"misc":"((translate 25 125) (scale 100 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 50 300) (scale 500 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 850 300) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 925 400) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 975 500) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 975 600) (scale 50 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 500 600) (scale 150 50) (texture #(struct:object:image% ...)))"}},[true]],[{"seal":{"misc":"((translate 99/4 55.287163517301785) (scale 101/2 171/2) (texture #(struct:object:bitmap% ...)))"}},[true]]]]],[10.0,[[],[],[[{"seal":{"misc":"((scale 4000 2250) (texture #(struct:object:image% ...)))"}},[true]]]]]]]]],[],[]]],[1,":observe",[[[1,":key-pressed",[[],[true],[]]],[1,":inbound",[[[2,":window",[[],[[],[true],[]],[]]],[4,":frame-event",[[],[[],[[],[[],[true],[]],[]],[]],[]]]],[],[]]]],[],[]]],[3,":on-screen-display",[[],[],[[-150.0,[[],[],[[10.0,[[],[],[[{"seal":{"misc":"#(struct:object:image% ...)"}},[true]]]]]]]]]]]],[],[]]],
    [[2,"scene-manager"],[[[1,":outbound",[[[2,":scene",[[],[],[[{"seal":{"misc":"((push-matrix (scale 600 400) (texture #(struct:object:image% ...))) (translate 0 0))"}},[[],[],[[{"seal":{"misc":"((translate 0 0) (push-matrix (translate 450 10) (scale 88 27) (texture #(struct:object:image% ...))))"}},[true]]]]]]]]],[],[]]],[1,":observe",[[[1,":scroll-offset",[[],[true],[]]],[1,":inbound",[[[2,":window",[[],[[],[true],[]],[]]],[3,":key-event",[[],[],[[{"misc":"f"},[[],[],[[true,[[],[true],[]]]]]]]]]],[],[]]],[3,":on-screen-display",[[],[[],[[],[true],[]],[]],[]]]],[],[]]]],[],[]]],
    [["meta",false],[[[1,":observe",[[[1,":outbound",[[],[true],[]]],[1,":observe",[[[1,":inbound",[[],[true],[]]]],[],[]]]],[],[]]],[1,":inbound",[[[2,":window",[[],[],[[600.0,[[],[],[[400.0,[true]]]]]]]]],[],[]]]],[],[]]],
    [[0,"drivers/timer"],[[[1,":observe",[[[3,":set-timer",[[],[[],[[],[true],[]],[]],[]]]],[],[]]],[1,":advertise",[[[2,":timer-expired",[[],[[],[true],[]],[]]]],[],[]]]],[],[]]]]]
];

var dataspaces = dataspace_blobs.map(function (j) {
  var result = {};
  j[2].forEach(function (p) {
    result[p[0][0]] = {
      name: p[0][1],
      assertions: Syndicate.Trie.trieFromJSON(p[1])
    };
  });
  return result
});

window.addEventListener('load', function () {
  var labels = [];
  var dsinfo = dataspaces[0];
  for (var k in dsinfo) { labels.push(k); }

  var matrix = [];
  labels.forEach(function (label1) {
    var row = [];
    labels.forEach(function (label2) {
      var r1 = dsinfo[label1].assertions;
      var r2 = dsinfo[label2].assertions;
      var v = Syndicate.Patch.biasedIntersection(r1, r2);
      if (Syndicate.Trie.is_emptyTrie(v)) {
        row.push(0);
      } else {
        row.push(1);
      }
    });
    matrix.push(row);
  });

  var svg = d3.select("svg"),
      width = +svg.attr("width"),
      height = +svg.attr("height"),
      outerRadius = Math.min(width, height) * 0.5 - 40,
      innerRadius = outerRadius - 30;

  var chord = d3.chord()
      .padAngle(0.05)
      .sortSubgroups(d3.descending);

  var arc = d3.arc()
      .innerRadius(innerRadius)
      .outerRadius(outerRadius);

  var ribbon = d3.ribbon()
      .radius(innerRadius);

  var color = d3.scaleOrdinal()
      .domain(d3.range(4))
      .range(["#000000", "#FFDD89", "#957244", "#F26223"]);

  var g = svg.append("g")
      .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")")
      .datum(chord(matrix));

  var group = g.append("g")
      .attr("class", "groups")
      .selectAll("g")
      .data(function(chords) { return chords.groups; })
      .enter().append("g");

  group.append("path")
    .style("fill", function(d) { return color(d.index); })
    .style("stroke", function(d) { return d3.rgb(color(d.index)).darker(); })
    .attr("d", arc);

  group.append("text")
    .each(function(d) { d.angle = (d.startAngle + d.endAngle) / 2; })
    .attr("dy", ".35em")
    .attr("transform", function(d) {
      return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
        + "translate(" + (outerRadius + 10) + ")"
        + "rotate(" + -(d.angle * 180 / Math.PI - 90) + ")";
    })
    .style("text-anchor", function(d) { return d.angle > Math.PI ? "end" : null; })
    .text(function(d) { return labels[d.index] + ':' + dsinfo[labels[d.index]].name; });

  // var groupTick = group.selectAll(".group-tick")
  //     .data(function(d) { return groupTicks(d, 1e3); })
  //     .enter().append("g")
  //     .attr("class", "group-tick")
  //     .attr("transform", function(d) { return "rotate(" + (d.angle * 180 / Math.PI - 90) + ") translate(" + outerRadius + ",0)"; });

  // groupTick.append("line")
  //   .attr("x2", 6);

  // groupTick
  //   .filter(function(d) { return d.value % 5e3 === 0; })
  //   .append("text")
  //   .attr("x", 8)
  //   .attr("dy", ".35em")
  //   .attr("transform", function(d) { return d.angle > Math.PI ? "rotate(180) translate(-16)" : null; })
  //   .style("text-anchor", function(d) { return d.angle > Math.PI ? "end" : null; })
  //   .text(function(d) { console.log(d); return JSON.stringify(d.value); });

  g.append("g")
    .attr("class", "ribbons")
    .selectAll("path")
    .data(function(chords) { return chords; })
    .enter().append("path")
    .attr("d", ribbon)
    .style("fill", function(d) { return color(d.target.index); })
    .style("stroke", function(d) { return d3.rgb(color(d.target.index)).darker(); });

  // // Returns an array of tick angles and values for a given group and step.
  // function groupTicks(d, step) {
  //   var k = (d.endAngle - d.startAngle) / d.value;
  //   return d3.range(0, d.value, step).map(function(value) {
  //     return {value: value, angle: value * k + d.startAngle};
  //   });
  // }
});
