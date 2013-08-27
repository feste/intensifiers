function add(a,b) {return a+b;}
function sum(arr) {return arr.reduce(add,0);}
function mean(arr) {return sum(arr)/arr.length;}
function sd(arr) {
  var mu = mean(arr);
  var squaredDiff = arr.map(function(x){return Math.pow((x-mu),2);});
  return Math.sqrt(sum(squaredDiff)/arr.length);
}

// 48 questions
// an X that costs value is expensive
// an X that costs epsilon less than an expensive X is expensive
var epsilonDeviations = [0.01, 0.1, 0.5, 0.1];
var valueDeviations = [0, 1, 2, 3];
var items = ["headphones", "sweater", "laptop", "coffee maker", "watch"];
var nEps = epsilonDeviations.length;
var nVals = valueDeviations.length;
var nItems = items.length;
var nQsPerItem = nEps + nVals;
var nQs = nQsPerItem * nItems;

function getEps(item, sigs) {
  if(humanPriors[item] == null) {console.log("ERROR 2");}
  var sig = sd(humanPriors[item]);
  var raw = sigs*sig;
  return fancyRound(raw);
}

function getVal(item, sigs) {
  if(humanPriors[item] == null) {console.log("ERROR 4");}
  var sig = sd(humanPriors[item]);
  var mu = mean(humanPriors[item]);
  raw = (sigs*sig) + mu;
  return fancyRound(raw);
}

function fancyRound(rawPrice) {
  if (rawPrice > 100) {
    var roundPrice = Math.round(rawPrice/100)*100 + ".00";
  } else if (rawPrice > 20) {
    var roundPrice = Math.round(rawPrice/10)*10 + ".00";
  } else if (rawPrice > 1) {
    var roundPrice = Math.round(rawPrice) + ".00";
  } else {
    var roundPrice = Math.round(rawPrice*100)/100;
  }
  return "$" + roundPrice;
}

//make a map from items to shuffled epsilons
var epsilons = {}
for (var i=0; i<nItems; i++) {
  var item = items[i];
  epsilons[item] = shuffle(epsilonDeviations);
}

//make a map from items to shuffled values
var values = {}
for (var i=0; i<nItems; i++) {
  var item = items[i];
  values[item] = shuffle(valueDeviations);
}

//make a map from items to order of presentation of epsilon/value questions
var orders = {}
for (var i=0; i<nItems; i++) {
  var item = items[i];
  var unshuffledOrd = [];
  for (var j=0; j<nEps; j++) {
    unshuffledOrd.push("eps");
  }
  for (var j=0; j<nVals; j++) {
    unshuffledOrd.push("val");
  }
  orders[item] = shuffle(unshuffledOrd);
}

var trialItems = [];
for (var i=0; i<nQsPerItem; i++) {
  trialItems = trialItems.concat(items);
}
var shuffledItems = shuffle(trialItems);

if (!(trialItems.length == nQs)) {console.log("Error 5");}

function caps(a) {return a.substring(0,1).toUpperCase() + a.substring(1,a.length);}
function uniform(a, b) { return ( (Math.random()*(b-a))+a ); }
function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }
function shuffle(v) { newarray = v.slice(0);for(var j, x, i = newarray.length; i; j = parseInt(Math.random() * i), x = newarray[--i], newarray[i] = newarray[j], newarray[j] = x);return newarray;} // non-destructive.
function article(item) {
  var art = {
    "sweater": "a",
    "headphones": "",
    "electric kettle": "an",
    "coffee maker": "a",
    "watch": "a",
    "laptop": "a",
  }
  if (art[item] == null) {console.log("ERROR 1");}
  return art[item];
};

function sg(item) {
  var singular = {
    "sweater": true,
    "headphones": false,
    "electric kettle": true,
    "coffee maker": true,
    "watch": true,
    "laptop": true,
  }
  if (singular[item] == null) {console.log("ERROR 3: " + item);}
  return singular[item];
}

function be(singular) { if (singular) {return "is";} else {return "are";} }

$(document).ready(function() {
  $("#nQs").html(nQs);
  showSlide("consent");
  $("#mustaccept").hide();
});

var experiment = {
  data: {questions:[]},
  
  instructions: function() {
    if (turk.previewMode) {
      $("#instructions #mustaccept").show();
    } else {
      showSlide("instructions");
      $("#begin").click(function() { experiment.trial(0); })
    }
  },
  
  trial: function(qNumber) {
    $("#trialerror").hide();
    showSlide("trial");
    var item = shuffledItems[qNumber];
    var qType = orders[item].shift();
    if (qType == "val") {
      var sigs = values[item].shift();
      var dollarAmt = getVal(item, sigs);
      if (sg(item)) {
        var statement = "<b>" + caps(article(item)) + " " + item + " that costs " +
                        dollarAmt + " is expensive.</b>";
      } else {
        var statement = "<b>" + caps(item) + " that cost " + dollarAmt +
                        " are expensive.</b>";
      }
    } else if (qType == "eps") {
      var sigs = epsilons[item].shift();
      var dollarAmt = getEps(item, sigs);
      if (sg(item)) {
        var statement = "<b>" + caps(article(item)) + " " + item + " that costs " +
                        dollarAmt + " less than an expensive " + item +
                        " is also expensive.</b>";
      } else {
        var statement = "<b>" + caps(item) + " that cost " + dollarAmt + " less " +
                        "than expensive " + item + " are also expensive.</b>";
      }
    } else {
      console.log("error 6");
      console.log(qType);
    }
    $("#statement").html(statement);
    $('.bar').css('width', ( 100*qNumber/nQs + "%"));
    $("#continue").click(function() {
      var responseRaw = $("#form").serialize();
		  if (responseRaw.length < 8) {
			  $("#trialerror").show();
		  } else {
			  $("#continue").unbind("click");
			  $('input[name=rating]').attr('checked',false);
        var response = responseRaw.split("=")[1];
        experiment.data.questions.push({
          qNumber:qNumber,
          qType:qType,
          dollarAmt:dollarAmt,
          sigs:sigs,
          item:item,
          response:response});
        if (qNumber + 1 < nQs) {
          experiment.trial(qNumber+1);
        } else {
          experiment.questionaire();
        }
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
        setTimeout(function() { turk.submit(experiment.data) }, 1000);
      }
    });
  }
}
  
