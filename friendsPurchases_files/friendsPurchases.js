function caps(a) {return a.substring(0,1).toUpperCase() + a.substring(1,a.length);}
function uniform(a, b) { return ( (Math.random()*(b-a))+a ); }
function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }
function shuffle(v) { newarray = v.slice(0);for(var j, x, i = newarray.length; i; j = parseInt(Math.random() * i), x = newarray[--i], newarray[i] = newarray[j], newarray[j] = x);return newarray;} // non-destructive.
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
//function article(w) {if (/^[aeiou].*$/.test(w)) {return "an";} else {return "a";}}

function article(w) {
  if (w == "headphones") {
    return "a pair of";
  } else {
    return "a";
  }
}
function be(w) {
  if (w == "headphones") {
    return "were";
  } else {
    return "was";
  }
}
function pron(w) {
  if (w == "headphones") {
    return "they";
  } else {
    return "it";
  }
}
var items = ["laptop", "watch", "coffee maker", "sweater", "headphones"];
var intensifiers = ["very", "extremely", "wildly", "really", "insanely",
                    "super", "crazy", "excessively", "enormously", "vastly",
                    "quite", "uncommonly", "horribly", "exceedingly",
                    "hugely", "terribly", ""];
var purchasers = getPurchasers();
var itemsPerS = 3;
var qsPerItem = intensifiers.length;
var nQs = qsPerItem*itemsPerS;
var subjItems = items.splice(0,itemsPerS);
var itemBank = [];
for (var i=0; i<itemsPerS; i++) {
  itemBank = itemBank.concat(rep(subjItems[i], qsPerItem));}
var myItems = shuffle(itemBank);
var myIntensifiers = shuffle(replist(intensifiers, itemsPerS));
var myPurchasers = shuffle(replist(purchasers, itemsPerS));
var myColors = shuffle(["DF0101", "31B404", "0404B4"]);
// B404AE FF8000/FFBF00 04B4AE/088A85
var color = {}
for (var i=0; i<itemsPerS; i++) {
  color[subjItems[i]] = myColors[i];}

var experimentStart
$(document).ready(function() {
  showSlide("consent");
  $("#mustaccept").hide();
  $("#nQs").html(nQs);
  experimentStart = Date.now();});

var experiment = {
  data: {items:subjItems,
         colors:myColors,
         questions:[],},
  
  instructions: function() {
    if (turk.previewMode) {
      $("#instructions #mustaccept").show();}
    else {
      showSlide("instructions");
      $("#begin").click(function() { experiment.trial(0); })}},
  
  trial: function(qNumber) {
    $('#trialError').hide();
    $('.bar').css('width', ( qNumber/nQs*100 + "%"));
    showSlide("trial");
    startTime = Date.now();
    var purchaser = myPurchasers[qNumber];
    var intensifier = myIntensifiers[qNumber];
    var item = myItems[qNumber];
    $("#trialText").html('<p>Your friend ' + purchaser.name + ' says, "I just' +
                         ' bought ' + article(item) + ' <b>' +
                         '<font color="' + color[item] + '">' + item +
                         '</b></font>. ' +
                         caps(pron(item)) + ' ' + be(item) + ' <b>' +
                         intensifier + ' expensive</b>."</p>' +
                         '<p>How much do you think ' + pron(item) + ' cost?<p>');
    $("#continue").click(function() {
      var price = $("#price").val();
      var isPrice = /^[0-9]*(\.[0-9])?[0-9]$/.test(price);
      if (isPrice) {
        endTime = Date.now();
        experiment.data.questions.push({name:purchaser.name,
                                        intensifier:intensifier,
                                        item:item,
                                        color:color[item],
                                        price:price,
                                        rt:endTime-startTime})
        $("#continue").unbind("click");
        $("#price").val("");
        if (qNumber + 1 < nQs) {
          experiment.trial(qNumber + 1);}
        else {
          experiment.questionaire();}}
      else {
        $("#trialError").show();}})},
  
  questionaire: function() {
    //disable return key
    $(document).keypress( function(event){
     if (event.which == '13') {
        event.preventDefault();}});
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
        experimentEnd = Date.now();
        experiment.data["minutes"] = (experimentEnd - experimentStart)/1000/60;
        setTimeout(function() { turk.submit(experiment.data) }, 1000);
      }
    });
  }
}

function getPurchasers() {
  var maleNames = shuffle(["James", "John", "Robert", "Michael", "William",
                           "David", "Richard", "Charles", "Tim"]);
  var femaleNames = shuffle(["Mary", "Patricia", "Linda", "Barbara", "Elizabeth",
                             "Jennifer", "Maria", "Susan", "Sally"]);
  var gender = rep("m", maleNames.length).concat(rep("f", femaleNames.length));
  var objPron = {"m":"him", "f":"her"};
  var subjPron = {"m":"he", "f":"she"};
  var purchasers = [];
  for (var i=0; i<gender.length; i++) {
    var g = gender[i];
    if (g == "m") {
      var purchaserName = maleNames.shift();
    } else if (g == "f") {
      var purchaserName = femaleNames.shift();
    } else {
      console.log("error1");
    }
    purchasers.push({name: purchaserName,
                     obj: objPron[g],
                     subj: subjPron[g]});
  }
  return(purchasers);
}
