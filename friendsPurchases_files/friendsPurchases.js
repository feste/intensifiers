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

var nonceWords = shuffle([
  "wug", "dax", "fep", "speff", "zib", "gub", "wost", "wock", "thog",
  "snim", "ript", "quog", "polt", "poch", "murp", "mork", "mopt", "monx",
  "mone", "moge", "lide", "hoff", "hisp", "hinx", "hife", "hett", "fraw",
  "fing", "fick", "blim", "zop", "blick"
]);

//var items = ["laptop", "watch", "coffee maker", "sweater", "headphones"];
var items = ["laptop"];
var deintensifiers = ["kind of", "sort of", "somewhat", "slightly",
                      "moderately", "a bit", "a tad", "NOVEL"];
var intensifiers = [
  "strikingly", "staggeringly", "shockingly", "profoundly", "particularly",
  "majorly", "uberly", "unbelievably", "supremely", "remarkably",
  "quite", "outright", "incredibly", "impossibly", "extremely",
  "exceedingly", "especially", "downright", "completely", "awfully",
  "amazingly", "absurdly", "abnormally", "really", "utterly",
  "very", "seriously", "dreadfully", "thoroughly", "NOVEL"
];/*["very", "extremely", "wildly", "really", "insanely",
                    "super", "crazy", "excessively", "enormously", "vastly",
                    "quite", "uncommonly", "horribly", "exceedingly",
                    "hugely", "terribly", "NOVEL"];*/
var people = getPurchasers();
var itemsPerS = 1;
var qsPerItem = intensifiers.length + deintensifiers.length + 1; //the +1 is for the base expensive form
var types = [];
for (var i=0; i<intensifiers.length; i++) {
  types.push("intensifier");
}
for (var i=0; i<deintensifiers.length; i++) {
  types.push("deintensifier");
}
types.push("base");
var nQs = qsPerItem*itemsPerS;
var subjItems = items.splice(0,itemsPerS);
var itemBank = [];
for (var i=0; i<itemsPerS; i++) {
  itemBank = itemBank.concat(rep(subjItems[i], qsPerItem));}
var myItems = shuffle(itemBank);
var myIntensifiers = shuffle(replist(intensifiers, itemsPerS));
var myDeintensifiers = shuffle(replist(deintensifiers, itemsPerS));
var myPeople = shuffle(replist(people, itemsPerS));
var myTypes = shuffle(replist(types, itemsPerS));
//var myColors = shuffle(["DF0101", "31B404", "0404B4"]);
// B404AE FF8000/FFBF00 04B4AE/088A85
/*var color = {}
for (var i=0; i<itemsPerS; i++) {
  color[subjItems[i]] = myColors[i];}*/

var experimentStart
$(document).ready(function() {
  showSlide("consent");
  $("#mustaccept").hide();
  $("#nQs").html(nQs);
  experimentStart = Date.now();});

var experiment = {
  data: {//items:subjItems,
         //colors:myColors,
         //questions:[]
        },
  
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
    var purchaser = myPeople.shift().name;
    var questioner = myPeople.shift().name;
    var intensifier = myIntensifiers[qNumber];
    var item = myItems[qNumber];
    var type = myTypes[qNumber];
    var degreeAdv;
    var isNonce = false;
    if (type == "intensifier") {
      degreeAdv = intensifiers.shift();
    } else if (type == "deintensifier") {
      degreeAdv = deintensifiers.shift();
    } else {
      degreeAdv = "";
    }
    var word
    if (degreeAdv == "NOVEL") {
      degreeAdv = nonceWords.shift() + "ly";
      isNonce = true;
    }
    if (type == "intensifier") {
      $("#trialText").html('<p>You hear the following conversation between your two friends.</p>'+
                           '<p> ' + purchaser + ': I just bought ' + article(item) + ' new ' + item +
                           '. <br/> ' + questioner + ': Cool! How much did it cost? <br/> ' +
                           purchaser + ': <b>It was more than just expensive. It was ' +
                           degreeAdv + ' expensive.</b></p>' +
                           '<p>How much do you think the ' + item + ' cost?</p>');
    } else if (type == "deintensifier") {
      $("#trialText").html('<p>You hear the following conversation between your two friends.</p>'+
                           '<p> ' + purchaser + ': I just bought ' + article(item) + ' new ' + item +
                           '. <br/> ' + questioner + ': Cool! How much did it cost? <br/> ' +
                           purchaser + ': <b>Not too much. It was only ' +
                           degreeAdv + ' expensive.</b></p>' +
                           '<p>How much do you think the ' + item + ' cost?</p>');
    } else {
      $("#trialText").html('<p>You hear the following conversation between your two friends.</p>'+
                           '<p> ' + purchaser + ': I just bought ' + article(item) + ' new ' + item +
                           '. <br/> ' + questioner + ': Cool! How much did it cost? <br/> ' +
                           purchaser + ': <b>It was expensive.</b></p>' +
                           '<p>How much do you think the ' + item + ' cost?</p>');
    }
    $("#continue").click(function() {
      var price = $("#price").val();
      var isPrice = /^[0-9]*(\.[0-9])?[0-9]$/.test(price);
      if (isPrice) {
        endTime = Date.now();
        experiment.data["trial" + qNumber] = { purchaser:purchaser,
                              questioner:questioner,
                              type:type,
                              item:item,
                              degreeAdv:degreeAdv,
                              isNonce:isNonce,
                              price:price,
                              rt:endTime-startTime };
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

function getPurchasers(g) {
  if (g == "m") {
    var maleNames = shuffle(["Adam", "Ben", "Colin", "David", "Erik",
                             "Felix", "Greg", "Henry", "Ian", "John",
                             "Kyle", "Leon", "Michael", "Nick", "Owen",
                             "Paul", "Quinn", "Rob", "Sam", "Tim",
                             "Vince", "William", "Xander", "Zach", "Alex",
                             "Brian", "Chris", "Danny", "Evan", "Fred",
                             "Geoff", "Hank", "Ivan", "Jake", "Kevin",
                             "Larence", "Mitch", "Nathan", "Oliver", "Patrick",
                             "Quentin", "Roger", "Sean", "Thomas", "Adrian",
                             "Billy", "Dorian", "Elliott", "George", "Harry",
                             "Josh", "Max", "Noah", "Phil", "Rick",
                             "Sid", "Toby", "Martin", "Aaron", "Hal",
                             "Steve", "James", "Charles"]);
    var objPron = "him";
    var subjPron = "he";
    var purchasers = [];
    for (var i=0; i<maleNames.length; i++) {
      var purchaserName = maleNames.shift();
      purchasers.push({name: purchaserName,
                       obj: objPron[g],
                       subj: subjPron[g]});
    }
    return(purchasers);
  } else {
    var maleNames = shuffle(["Adam", "Ben", "Colin", "David", "Erik",
                             "Felix", "Greg", "Henry", "Ian", "John",
                             "Kyle", "Leon", "Michael", "Nick", "Owen",
                             "Paul", "Quinn", "Rob", "Sam", "Tim",
                             "Vince", "William", "Xander", "Zach", "Alex",
                             "Brian", "Chris", "Danny", "Evan", "Fred",
                             "Geoff", "Hank", "Ivan", "Jake", "Kevin",
                             "Larence", "Mitch", "Nathan", "Oliver", "Patrick",
                             "Quentin", "Roger", "Sean", "Thomas", "Adrian",
                             "Billy", "Dorian", "Elliott", "George", "Harry",
                             "Josh", "Max", "Noah", "Phil", "Rick",
                             "Sid", "Toby", "Martin", "Aaron", "Hal",
                             "Steve", "James", "Charles"]);
    var femaleNames = shuffle(["Anna", "Beth", "Clara", "Danielle", "Emma",
                               "Fiona", "Gina", "Hillary", "Isabel", "Jessica",
                               "Kim", "Laura", "Meagan", "Nina", "Patricia",
                               "Rachel", "Sarah", "Trisha", "Violet", "Abby",
                               "Briana", "Chloe", "Dana", "Erin", "Gabby",
                               "Gwen", "Sally", "Becca", "Tanya", "Mia",
                               "Susan", "Veronica", "Jennifer", "Mary", "Linda",
                               "Lydia", "Vanessa", "Eliza", "Barbara", "Alyssa",
                               "Stephanie", "Georgia", "Tina", "Margaret", "Amy"]);
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
}
