var grey = "#e6e6e6";

function darken(origColor, eps) {
  var eps = eps || 0.1;
  var c = Raphael.color(origColor);
  if (c.v - eps > 0) {
    var value = c.v - eps;
  } else {
    var value = 0;
  }
  var newColor = Raphael.hsb2rgb(c.h, c.s, value);
  return newColor.hex;
}


function lighten(origColor, saturation) {
  var saturation = saturation || false;
  var eps = 0.2;
  var c = Raphael.color(origColor);
  if (c.v + eps < 1) {
    var value = c.v + eps;
  } else {
    var value = 1;
  }
  if (saturation) {
    var saturationEps = 0.1;
    if (c.s - saturationEps > 0) {
      var sat = c.s - saturationEps;
    } else {
      var sat = 0;
    }
  } else {sat = c.s;}
  var newColor = Raphael.hsb2rgb(c.h, sat, value);
  return newColor.hex;
}

function makeGradient(intro, origColor) {
  var light = lighten(origColor);
  var dark = darken(origColor);
  console.log(light);
  console.log(dark);
  var grad = intro + light + "-" + dark;
  return grad;
}



function rep(x, n) {
  newlist=[];
  for(var i=0;i<n;i++) {
    newlist.push(x);
  }
  return(newlist);
}
function replist(list, n) {
  newlist = [];
  for (var i=0; i<n; i++) {
    newlist = newlist.concat(list);
  }
  return(newlist);
}

var nonceWords = ["feps", "wugs", "tigs", "daxes", "blicks", "speffs",
                  "zibs", "gubs", "borts"];

var pairings = [ ["crown", "cloudedMoon"], ["ghost", "s"],
                 ["splat", "flyingSquirrel"], ["weirdPacman", "weirdHeart"]];
var paper = Raphael("twoShapes", 600, 150);

var comparisons = []
for (var i=0; i<5; i++) {
  comparisons.push( [i*2/10, (i+1)*2/10] );
}
for (var i=0; i<4; i++) {
  comparisons.push( [i*2/10, (i+2)*2/10] );
}
for (var i=0; i<3; i++) {
  comparisons.push( [i*2/10, (i+3)*2/10] );
}
comparisons.map(function(x){console.log(x);});

var artifactsPerSubj = 4;
var comparisonsPerArtifact = comparisons.length;

var myComparisons;
/*for (var i=0; i<artifactsPerSubj; i++) {
  myComparisons = myComparisons.concat(shuffle(comparisons.map(shuffle)));
}*/

var artifacts = shuffle(nonceWords).splice(0, artifactsPerSubj);
//var uniqueEndpointNames = shuffle(pairings).splice(0,artifactsPerSubj);
//var endpointNames = shuffle(replist(uniqueEndpointNames, myComparisons.length));
var endpointNames = shuffle(pairings).splice(0, artifactsPerSubj);

var nQs = artifactsPerSubj*comparisonsPerArtifact;

var experimentStart;

// creates an image partway between two other images, as in the animations
// by Raphael.js
function intermediate(from, to, prop) {
  var fromCurve = Raphael.path2curve(from);
  var toCurve = Raphael.path2curve(to);
  var diff = [];
  var attr = "path";
  //compute difference between paths and store in diff
  for (i = 0, ii = fromCurve.length; i < ii; i++) {
    diff[i] = [0];
    for (var j = 1, jj = fromCurve[i].length; j < jj; j++) {
      diff[i][j] = (toCurve[i][j] - fromCurve[i][j]);}}
  var S = " ";
  now = [];
  //compute new path string for intermediate image
  for (var i = 0, ii = fromCurve.length; i < ii; i++) {
    now[i] = [fromCurve[i][0]];
    for (var j = 1, jj = fromCurve[i].length; j < jj; j++) {
      now[i][j] = +fromCurve[i][j] + prop * diff[i][j];}
    now[i] = now[i].join(S);}
  return now.join(S);}

function caps(a) {return a.substring(0,1).toUpperCase() + a.substring(1,a.length);}
function uniform(a, b) { return ( (Math.random()*(b-a))+a ); }
function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }
function shuffle(v) { newarray = v.slice(0);for(var j, x, i = newarray.length; i; j = parseInt(Math.random() * i), x = newarray[--i], newarray[i] = newarray[j], newarray[j] = x);return newarray;} // non-destructive.

$(document).ready(function() {
  showSlide("consent");
  $("#mustaccept").hide();
  experimentStart = Date.now();
});

var experiment = {
  data: {questions:[]},
  
  instructions: function() {
    if (turk.previewMode) {
      $("#instructions #mustaccept").show();
    } else {
      showSlide("instructions");
      $("#begin").click(function() { experiment.trialIntro(0, 0); })
    }
  },
  
  trialIntro: function(artifactNumber) {
    var artifactName = artifacts[artifactNumber];
    if (artifactNumber == 0) {
      var firstOrNext = "first";
    } else {
      var firstOrNext = "next";
    }
    myComparisons = shuffle(comparisons.map(shuffle));
    $("#trialIntroText").html("<p>The " + firstOrNext + " kind of artifact " +
                              "that we'd like " +
                              "you to help us study are called <b>" +
                              artifactName + "</b>.</p>" +
                              "<p>Click continue when you are ready to start " +
                              "looking at " + artifactName + ".</p>");
    showSlide("trialIntro");
    $("#beginTrialSet").click(function() {
      $("#beginTrialSet").unbind("click");
      experiment.trial(0, artifactNumber);
    })
  },
  
  trial: function(qNumber, artifactNumber) {
    artifactName = artifacts[artifactNumber];
    $("#artifactName").html(artifactName);
    var qsSoFar = comparisonsPerArtifact*artifactNumber + qNumber;
    $('.bar').css('width', ( qsSoFar/nQs*100 + "%"));
    showSlide("trial");
    paper.clear();
    var startTime = Date.now();
    var comparison = myComparisons.shift();
    var shapeNames = endpointNames[artifactNumber];
    var shapePaths = shapeNames.map(function(n) {return shapes[n];});
    var onScreenShapes = comparison.map(function(prop) {
      return paper.path(intermediate(shapePaths[0], shapePaths[1], prop));
    })
    onScreenShapes.map(function(shape) {
      shape.attr({fill:makeGradient("r", grey)});
    })
    onScreenShapes[0].transform("T80,-90S0.3");
    onScreenShapes[1].transform("T220,-90S0.3");
    var actualDistance = Math.abs(comparison[0] - comparison[1]);
    $("#trialerror").hide();
    $("#continue").click(function() {
      var raw = $("form").serialize();
      var response = raw.split("&")[0].split("=")[1];
      if (response.length > 0) {
        var endTime = Date.now();
        experiment.data.questions.push({comparison:comparison,
                                        response:response,
                                        actualDistance:actualDistance,
                                        rt:endTime - startTime});
			  $('input[name=rating]').attr('checked',false);
			  $("#continue").unbind("click");
        if (qNumber + 1 < comparisonsPerArtifact) {
          experiment.trial(qNumber + 1, artifactNumber);
        } else if (artifactNumber + 1 < artifactsPerSubj) {
          experiment.trialIntro(artifactNumber + 1);
        } else {
          experiment.questionaire();
        }
      } else {
        $("#trialerror").show();
      }
    })
  },
  
  questionaire: function() {
    //disable return key
    $(document).keypress( function(event){
     if (event.which == '13') {
        event.preventDefault();
      }
    });
    //progress bar complete
    $('.bar').css('width', ( "100%"));
    showSlide("questionaire");
    $("#formsubmit").click(function() {
      rawResponse = $("#questionaireform").serialize();
      pieces = rawResponse.split("&");
      var age = pieces[0].split("=")[1];
      var lang = pieces[1].split("=")[1];
      var comments = pieces[2].split("=")[1];
      if (lang.length > 0) {
        experiment.data["language"] = lang;
        experiment.data["comments"] = comments;
        experiment.data["age"] = age;
        showSlide("finished");
        var experimentEnd = Date.now();
        experiment.data["minutes"] = (experimentEnd - experimentStart)/1000/60;
        setTimeout(function() { turk.submit(experiment.data) }, 1000);
      }
    });
  }
}
  
