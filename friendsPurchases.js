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
var items = shuffle(["laptop", "watch", "coffee maker"])//, "sweater", "headphones"]);
var intensifiers = shuffle([
  "very", "extremely", "wildly", "really", "insanely",
  "super", "crazy", "excessively", "enormously", "vastly",
  "quite", "uncommonly", "horribly", "exceedingly",
  "hugely", "terribly", "pretty", "supremely", "ridiculously", "radically",
  "phenomenally", "outrageously", "fantastically", "extraordinarily", "ever so",
  "amazingly", "astoundingly", "crazily", "real", "stupidly",
  "totally", "uber", "intensely", "truly", "decidedly",
  "highly", "rather", "greatly"
]);
var deintensifiers = shuffle([
  "kind of", "sort of", "kinda", "a tad", "a bit",
  "sorta", "slightly", "somewhat", "moderately", "barely",
  "fairly", "a little", "reasonably", "relatively",
]);

var colors = shuffle(["DF0101", "31B404", "0404B4"]);
var color = {}
for (var i=0; i<items.length; i++) {
  color[items[i]] = colors[i];}

var nItems = items.length;
var nIntensifiers = intensifiers.length
var nDeintensifiers = deintensifiers.length

var nQsPerItem = 1 + nIntensifiers + nDeintensifiers
var nQs = nItems * nQsPerItem

var myItems_unshuf = []
for (var i=0; i<nItems; i++) {
  myItems_unshuf = myItems_unshuf.concat(rep(items[i], nQsPerItem))
}
var myWords_unshuf = []
for (var i=0; i<nItems; i++) {
  myWords_unshuf = myWords_unshuf.concat([""].concat(intensifiers.concat(deintensifiers)))
}
var myPurchasers = getPurchasers(nQs);

var indices = []
for (var i=0; i<nQs; i++) {
  indices.push(i);
}
var myIndices = shuffle(indices);

var myWords = [];
var myItems = [];
for (var i=0; i<nQs; i++) {
  var index = myIndices[i];
  myWords.push(myWords_unshuf[index]);
  myItems.push(myItems_unshuf[index])
}

/*var purchasers = getPurchasers();
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
  color[subjItems[i]] = myColors[i];}*/

var experimentStart
$(document).ready(function() {
  showSlide("consent");
  $("#mustaccept").hide();
  $("#nQs").html(nQs);
  experimentStart = Date.now();});

var experiment = {
  data: {items:items,
         colors:colors,
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
    var intensifier = myWords[qNumber]; //not actually an intensifier anymore
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
                                        gender:purchaser.gender,
                                        word:intensifier,
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
        experiment.data["version"] = "1-24";
        showSlide("finished");
        experimentEnd = Date.now();
        experiment.data["minutes"] = (experimentEnd - experimentStart)/1000/60;
        setTimeout(function() { turk.submit(experiment.data) }, 1000);
      }
    });
  }
}

function getPurchasers(n) {
  var maleNames = shuffle(["Aaron", "Adam", "Adrian", "Aiden", "Alan", "Albert", "Alex", "Alfred", "Alvin", "Andrew", "Anthony", "Arnold", "Arthur", "Austin", "Ayden", "Barry", "Ben", "Bentley", "Bernard", "Bill", "Blake", "Bob", "Bradley", "Brandon", "Brayden", "Brent", "Brett", "Brian", "Brody", "Bruce", "Bryson", "Byron", "Caleb", "Calvin", "Cameron", "Carl", "Carson", "Carter", "Casey", "Chad", "Charlie", "Chase", "Chester", "Chris", "Clarence", "Claude", "Clayton", "Clifford", "Clifton", "Clinton", "Clyde", "Cody", "Colin", "Colton", "Connor", "Cooper", "Cory", "Craig", "Curtis", "Dale", "Damian", "Danny", "Darren", "Daryl", "Dave", "Dennis", "Derrick", "Dominic", "Donald", "Dorian", "Douglas", "Dustin", "Dwayne", "Dwight", "Dylan", "Easton", "Eddie", "Edgar", "Edwin", "Eli", "Elijah", "Elliott", "Eric", "Ernest", "Ethan", "Eugene", "Evan", "Everett", "Felix", "Floyd", "Frank", "Fred", "Gabriel", "Gary", "Gavin", "Gene", "George", "Gerald", "Gilbert", "Glen", "Gordon", "Grayson", "Greg", "Harold", "Harry", "Harvey", "Hector", "Henry", "Herbert", "Herman", "Howard", "Hudson", "Hugh", "Hunter", "Ian", "Isaac", "Isaiah", "Ivan", "Jace", "Jack", "Jackson", "Jacob", "James", "Jared", "Jason", "Jaxon", "Jay", "Jayden", "Jeff", "Jeremiah", "Jeremy", "Jerome", "Jerry", "Jesse", "Jim", "Joe", "Joel", "John", "Jonathan", "Jordan", "Jose", "Joseph", "Josh", "Joshua", "Josiah", "Juan", "Julian", "Julius", "Justin", "Kayden", "Keith", "Ken", "Kent", "Kevin", "Kurt", "Kyle", "Lance", "Landon", "Larry", "Lawrence", "Lee", "Leo", "Leon", "Leonard", "Leroy", "Lester", "Levi", "Liam", "Lloyd", "Logan", "Lonnie", "Louis", "Lucas", "Luis", "Luke", "Mark", "Marshall", "Martin", "Marvin", "Mason", "Matthew", "Maurice", "Max", "Melvin", "Michael", "Milton", "Mitchell", "Morris", "Nathan", "Nathaniel", "Neil", "Nelson", "Nicholas", "Noah", "Nolan", "Norman", "Oliver", "Oscar", "Owen", "Parker", "Patrick", "Paul", "Perry", "Peter", "Phillip", "Quentin", "Ralph", "Randy", "Ray", "Raymond", "Rick", "Robert", "Rodney", "Roger", "Roland", "Ron", "Ross", "Roy", "Ruben", "Russell", "Ryan", "Ryder", "Sam", "Samuel", "Scott", "Sean", "Sebastian", "Seth", "Shane", "Stanley", "Steven", "Ted", "Terrence", "Theodore", "Thomas", "Tim", "Todd", "Tony", "Travis", "Tristan", "Troy", "Tyler", "Tyrone", "Uri", "Vernon", "Victor", "Vincent", "Wade", "Wallace", "Walter", "Warren", "Wayne", "Wesley", "Willard", "William", "Willie", "Wyatt", "Xavier", "Yakov", "Zachary"]);
  if (maleNames.length < n) {
    console.log("error 99");
  }
  /*var maleNames = shuffle(["James", "John", "Robert", "Michael", "William",
                           "David", "Richard", "Charles", "Tim"]);
  var femaleNames = shuffle(["Mary", "Patricia", "Linda", "Barbara", "Elizabeth",
                             "Jennifer", "Maria", "Susan", "Sally"]);
  var gender = rep("m", maleNames.length).concat(rep("f", femaleNames.length));*/
  var gender = rep("m", n);
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
                     subj: subjPron[g],
                     gender: g});
  }
  return(purchasers);
}
