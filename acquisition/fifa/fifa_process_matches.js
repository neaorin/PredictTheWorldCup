let jsdom = require('jsdom/lib/old-api');
let fs = require('fs');
let mkdirp = require('mkdirp');
let competitions = require('./processed/competitions/competitions.json');


function processMatchFile(filePath)
{

  let outputFilePath = filePath.replace('raw','processed').replace('.html','.json');
    if(fs.existsSync(outputFilePath)) {
    console.log(outputFilePath + " exists.");
    return 0;
  }
  
  console.log('Processing ' + filePath);
  let outputDirPath = outputFilePath.substring(0, outputFilePath.lastIndexOf('/'));
  mkdirp.sync(outputDirPath);

  jsdom.env({
    file: filePath,
    scripts: [
      'https://code.jquery.com/jquery-2.1.4.min.js'
    ],
    done: function (err, window) {
      if (err != null) {
        console.log(err);
        return null;
      }
      let $ = window.jQuery;
  
      let idCupSeason = filePath.match(/^\.\/raw\/matches\/(.*)\/.*$/)[1];
          
      let results = $("div.mc-match-is-result").map(function() {
           try {
            let match = {
                date: $(this).attr("data-matchdate"),
                team1: $(this).find("div.home div.t-i img.i-3-flag").attr('class').substring(0, 3),
                team1Text: $(this).find("div.home span.t-nText").text().trim(),
                team2: $(this).find("div.away div.t-i img.i-3-flag").attr('class').substring(0, 3),
                team2Text: $(this).find("div.away span.t-nText").text().trim(),
                resText: $(this).find("div.s span.s-resText").text().trim(),
                statText: $(this).find("div.s span.s-statText").text().trim(),
                venue: $(this).find("div.m-venue span.m-venueText").text().trim(),
                IdCupSeason: idCupSeason
            };
            let compids = competitions.filter(function(item) {
                return (item.IdCupSeason == match.IdCupSeason);
            });
            if (compids.length != 0) {
                match.CupName = compids[0].CupName;
            }
            
            let scoreMatches = match.resText.match(/^(.*)-(.*).*\((.*)-(.*)\)$/);
            if (scoreMatches != null && scoreMatches.length == 5) {
                match.team1Score = scoreMatches[1];
                match.team2Score = scoreMatches[2];
                match.team1PenScore = scoreMatches[3];
                match.team2PenScore = scoreMatches[4];          
            }
            else {
                scoreMatches = match.resText.match(/^(.*)-(.*).*$/);
                if (scoreMatches != null && scoreMatches.length == 3) {
                match.team1Score = scoreMatches[1];
                match.team2Score = scoreMatches[2];         
                }
            }
            return match;
          }
          catch (ex) {
              console.error("Error processing file " + filePath + " : " + ex);
              return null;
          }
          
      }).get();
      
      fs.writeFileSync(outputFilePath, JSON.stringify(results));
    }
  });
  
}

let walk = function(dir) {
    let results = [];
    let list = fs.readdirSync(dir);
    list.forEach(function(file) {
        file = dir + '/' + file
        let stat = fs.statSync(file);
        if (stat && stat.isDirectory()) results = results.concat(walk(file));
        else results.push(file);
    })
    return results;
} 

// individual files - raw html to processed json files
function processRawFiles() {
    let files = walk('./raw/matches');
    files.forEach(function(file) {
        processMatchFile(file);
    });
}



// concatenate json files into one file
function concatenateProcessedFiles() {
    // delete output file so it doesn't get processed as input
    let outputFilePath = './processed/matches/matches.json';
    if(fs.existsSync(outputFilePath)) {
        fs.unlinkSync(outputFilePath);
    }

    let files = walk('./processed/matches');
    let matches = new Array();
    files.forEach(function(file) {
        if (file.endsWith('.json')) {
            let filematches = require(file);
            matches = matches.concat(filematches);
        }
    });
    fs.writeFileSync(outputFilePath, JSON.stringify(matches, null, 3));
}


// check how many files are raw vs processed
function checkFiles() {
    console.log(walk('./raw/matches').length);
    console.log(walk('./processed/matches').length);
}


// main
if (process.argv.length <= 2)
    console.error("Must supply action: process, concat, check");
else {
    switch (process.argv[2]) {
        case "process": 
            processRawFiles();
            break;
        case "concat":
            concatenateProcessedFiles();
            break;
        case "check":
            checkFiles();
            break;
        default:
            console.error("Action not understood.")
    }
}
