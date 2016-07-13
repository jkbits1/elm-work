/**
 * Created by Jon on 24/06/16.
 */

"use strict";

// passing multiple arrays signal to d3 to support multiple
// circles
function circles() {
  return [
    // {name: "c1", value: 1, dataItems: donutDataList[0]}
    // , {name: "c2", value: 2, dataItems: donutDataList[1]}
    // donutDataList[0], donutDataList[1]
      ["a", "b", "c"]
    , ["m", "n", "o"]
    , ["p", "q", "r"]
    , ["x", "y", "z"]
  ]
}

var svg;

function showCircle (donutDataList) {

//Create a color scale
  var colorScale = d3.scale.linear()
    // .domain([1, 3.5, 6])
    // .domain([1, 4.8, 6])
    // .domain([0.2, 2.8, 6])
    // .domain([0.03, 2.6, 6])
    .domain([0.03, 2.1, 6])
    .range([
      // "#2c7bb6",
      "#3c7bb6",
      // "#ffffbf",
      "#D8D865",
      "#d7191c"])
    .interpolate(d3.interpolateHcl);

//Create an arc function
  var arc = d3.svg.arc()
      .innerRadius(function (d) {
        return d.data.sz * 0.1 * width * 0.75 / 2;
      })
      .outerRadius(function (d) {
        return d.data.sz * 0.1 * width * 0.75 / 2 + 30;
      })
  // .cornerRadius(6)
    ;

  var screenWidth = window.innerWidth;

  var initWH = 400;

  // this seems to work pretty well, a bit iffy for 2.0 upwards though
  // var adjRatio = 0.7;
  var adjRatio = 1.0;
  // var adjRatio = 1.7;
  // var adjRatio = 2.2;

  var margin = {left: 20, top: 20, right: 20, bottom: 20},
    // width = Math.min(screenWidth, 500) - margin.left - margin.right,
    // height = Math.min(screenWidth, 500) - margin.top - margin.bottom;

  width = Math.min(screenWidth, 500 * adjRatio) - margin.left - margin.right,
  height = Math.min(screenWidth, 500 * adjRatio) - margin.top - margin.bottom;

  svg = d3.select("#chart svg").remove();

  var svgWidth  = 400; //(width + margin.left + margin.right);
  var svgHeight = 400; // (height + margin.top + margin.bottom);

  var transWidth  = 200; // ((width / 2 + margin.left) - 50)
  var transHeight = 175; // ((height / 2 + margin.top) - 50)

  // var
  svg = d3.select("#chart").append("svg")
    .attr("width", svgWidth )
    .attr("height", svgHeight)
    .append("g").attr("class", "wrapper")
    .attr("transform",
      "translate(" +
       transWidth + "," +
        transHeight + ")")
    // .attr("transform", "translate(" + (width + margin.left) + "," + (height + margin.top) + ")")
    //   .attr("transform", "translate(250,250)")
    ;

  svg = svg.selectAll("g")
    .data(circles)
    // .data(circles, d => {
    //   return d.name;
    // })
    .enter().append("g")
    .attr("class", "circles")
    ;

  // NOTE set MATRIX
  // https://github.com/d3/d3-3.x-api-reference/blob/master/Selections.md


//Turn the pie chart 90 degrees counter clockwise, so it starts at the left
  var pie = d3.layout.pie()
    .startAngle(-90 * Math.PI / 180)
    .endAngle(-90 * Math.PI / 180 + 2 * Math.PI)
    .value(
      d => {
        return d.value;
      }
    )
    .padAngle(.01)
    .sort(null);

  function getDataList (d) {
    var n = 3;

    if (d[0] == "a") {
      n = 0;
    }
    else if (d[0] == "m") {
      n = 1;
    }
    else if (d[0] == "p") {
      n = 2;
    }

    return pie(donutDataList[n]);
  }

  donuts();

  function donuts ()
  {
//Create the donut slices

  svg.selectAll(".donutArcSlices")
    // .selectAll(".donutArcSlices")
    .data(getDataList)
    .enter().append("path")
    .attr("class", "donutArcSlices")

    // animals, left aligned
    // .attr("id", function(d,i) { return "donutArc"+i; })

    // animals, centered, left aligned, flipped
    .attr("d", arc)
    .style("fill", (d, i) => {
      if (i === 7) return "#CCCCCC"; //Other
      else return colorScale(i);
    })
    // animals, left aligned
    // ;

    // animals, centered (section of this code below flips the lower text)
    //
    // note - can't convert this to fat arrow syntax,
    // something inside function gets broken, maybe regex syntax issue?
    .each(function (d, i) {

      //A regular expression that captures all in between the start of a string (denoted by ^) and a capital letter L
      //The letter L denotes the start of a line segment
      //The "all in between" is denoted by the .+?
      //where the . is a regular expression for "match any single character except the newline character"
      //the + means "match the preceding expression 1 or more times" (thus any single character 1 or more times)
      //the ? has to be added to make sure that it stops at the first L it finds, not the last L
      //It thus makes sure that the idea of ^.*L matches the fewest possible characters
      //For more information on regular expressions see: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
      var firstArcSection = /(^.+?)L/;

      //Grab everything up to the first Line statement
      //The [1] gives back the expression between the () (thus not the L as well) which is exactly the arc statement
      var newArc;
      var firstArcArray = firstArcSection.exec(d3.select(this).attr("d"));
      
      if (firstArcArray == null) {
        newArc = "";
      }
      else {
        newArc = firstArcArray[1];
      }
      //Replace all the comma's so that IE can handle it -_-
      //The g after the / is a modifier that "find all matches rather than stopping after the first match"
      newArc = newArc.replace(/,/g, " ");

      // flips the text
      //If the end angle lies beyond a quarter of a circle (90 degrees or pi/2)
      //flip the end and start position
      if (d.endAngle > 90 * Math.PI / 180) {
        var startLoc = /M(.*?)A/,		//Everything between the first capital M and first capital A
          middleLoc = /A(.*?)0 0 1/,	//Everything between the first capital A and 0 0 1
          endLoc = /0 0 1 (.*?)$/;	//Everything between the first 0 0 1 and the end of the string (denoted by $)

        //Build up the new arc notation, set the sweep-flag to 0
        if (firstArcArray == null) {
            newArc = "";
        }
        else {
            //Flip the direction of the arc by switching the start en end point (and sweep flag)
            //of those elements that are below the horizontal line
            var newStart = endLoc.exec(newArc)[1];
            var newEnd = startLoc.exec(newArc)[1];
            var middleSec = middleLoc.exec(newArc)[1];
        
            newArc = "M" + newStart + "A" + middleSec + "0 0 0 " + newEnd;
        }
      }//if

      //Create a new invisible arc that the text can flow along
      svg.append("path")
        .attr("class", "hiddenDonutArcs")
        .attr("id", "donutArc" + i)
        .attr("d", newArc)
        .style("fill", "none");

    });

    // text, left aligned, centered, flipped (see specific sections below)
    //Append the label names on the outside
    svg.selectAll(".donutText")
    // left aligned and centered
    //   .data(donutData)

    // flipped
    .data( getDataList )

    // left aligned, centered, flipped
    .enter().append("text")
    .attr("class", "donutText")

    // left aligned and centered
    // .attr("dy", -13)

    // flipped
    //Move the labels below the arcs for those slices with an end angle greater than 90 degrees
    .attr("dy", (d, i) => {
      var dy = (d.endAngle > 90 * Math.PI / 180 ? 18 : -11);

      var adjDy = dy;
      // var sz2adj = 37;
      // var sz2adj2 = 9.6;
      var sz4adj = 28;
      var sz2adj = 38.5;
      var sz2adj2 = 6.8;

      if (dy < 0) {
        if (d.data.sz == 6) {
          adjDy = (-0.3 * dy) * 17;
        }
        else if (d.data.sz == 4) {
          adjDy = (-0.3 * dy) * sz4adj;
        }
        else if (d.data.sz == 2) {
          adjDy = (-0.3 * dy) * sz2adj;
        }
        else {
          adjDy = dy * -1.9;
        }
      }
      else {
        if (d.data.sz == 6) {
          adjDy = (-0.9 * dy) * 2.4;
        }
        else if (d.data.sz == 4) {
          adjDy = (-0.9 * dy) * 4.8;
        }
        else if (d.data.sz == 2) {
          adjDy = (-0.9 * dy) * sz2adj2;
        }
        else {
          adjDy = dy * -0.44;
        }
      }

      adjDy = adjDy * adjRatio;

      return adjDy;
    })

    // left aligned, centered, flipped
    .append("textPath")
    // animals, left aligned
    // .attr("xlink:href",function(d,i){return "#donutArc"+i;})
    // .text( d => d.name );

    // animals, centered, flipped
    .attr("startOffset", "50%")
    .style("text-anchor", "middle")
    .attr("xlink:href", (d, i) => "#donutArc" + i)

    // animals, centered
    // .text(d => d.name);

    // animals, flipped
    .text(d => d.data.name);

}
}