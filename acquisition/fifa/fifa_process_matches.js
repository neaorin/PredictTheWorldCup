let fs = require("fs")
let moment = require("moment")
let mkdirp = require('mkdirp');

let cities = require('./cities.json');

// FIFA-sanctioned competitions which you are NOT interested in. These will not be included in the processed dataset
const skippedTournaments = [ 'beach', 'futsal', 'u17', 'u-17', 'u-20', 'club final', 'club world cup', 'club world championship', 'youth olympic' ]

let processedMatches = []

let processMatchFile = function(filename) {
	let contents = fs.readFileSync(filename);
 	let jsonContent = JSON.parse(contents);

 	for (competitionId in jsonContent.competitionslist) {
 		const competition = jsonContent.competitionslist[competitionId];

 		let shouldSkip = false;

 		// skip non-official grass football games
 		for (idx in skippedTournaments) {
	 		if (competition.name.toLowerCase().indexOf(skippedTournaments[idx]) != -1) { 
	 			shouldSkip = true;
	 			break;
	 		}

	 		if (competition.competitionSeoName.toLowerCase().indexOf(skippedTournaments[idx]) != -1) { 
	 			shouldSkip = true;
	 			break;
	 		}

 		}

 		// skip club competitions
 		if (!shouldSkip && competition.isClubCompetition === true) {
 			shouldSkip = true;
 		}

 		if (shouldSkip) {
 			console.log(`Skipping ${competition.name} [${competition.competitionSeoName}] ...`);
 			continue;
 		}

		console.log(`Processing ${competition.name} [${competition.competitionSeoName}] ...`);
		processTournament(competition)
 	}
}

let processTournament = function(tournamentJson) {
	for (gameJsonId in tournamentJson.matchlist) {
		gameJson = tournamentJson.matchlist[gameJsonId]

		if (!gameJson.isFinished) {
			continue;
		}

		let rawDate = gameJson.matchDate

		if (gameJson.hasOwnProperty('matchDateUTC')) {
			rawDate = gameJson.matchDateUTC
		}

		let date = moment(rawDate);
		let dateComponent = date.utc().format('YYYYMMDD');

		let venue = ''
		if (gameJson.hasOwnProperty('venueName')) {
			venue = gameJson.venueName.trim()
			let cityData = cities.find(x => x.city == venue);
			if (cityData) 
				venue = `${venue}, ${cityData.country}`;
		} 

		let processedMatch = {
	      "date": dateComponent,
	      "team1": gameJson.homeCountryCode,
	      "team1Text": gameJson.homeTeamName,
	      "team2": gameJson.awayCountryCode,
	      "team2Text": gameJson.awayTeamName,
	      "venue": venue,
	      "IdCupSeason": gameJson.idCupSeason.toString(),
	      "CupName": gameJson.cupKindName,
	      "team1Score": gameJson.scoreHome,
	      "team2Score": gameJson.scoreAway,		
		}

		// reasonWinCode values from: http://js.fifa.com/components/script/require-libs/lang=en/main.js
		// for our purposes only 3 is important enough

		// "fifa.winningReason_0":{text:"Draw", abbr:"Draw"},
		// "fifa.winningReason_2":{text:"{WinTeamName} win after extra time", abbr:"{WinTeamName} win AET"},
		// "fifa.winningReason_3":{text:"{WinTeamName} win on penalties ({ScorePenH} - {ScorePenA})", abbr:"{WinTeamCountry} win after PSO"},
		// "fifa.winningReason_4":{text:"{WinTeamName} win on aggregate after regular time ({ScoreAggH} - {ScoreAggA})", abbr:"{WinTeamCountry} Win on agg."},
		// "fifa.winningReason_5":{text:"{WinTeamName} win on aggregate after extra time ({ScoreAggH} - {ScoreAggA})", abbr:"{WinTeamCountry} win on agg AET"},
		// "fifa.winningReason_6":{text:"{WinTeamName} win on away goal after regular time", abbr:"{WinTeamCountry} win on away goals"},
		// "fifa.winningReason_7":{text:"{WinTeamName} win on away goal after extra time", abbr:"{WinTeamCountry} win on away goals AET"},
		// "fifa.winningReason_8":{text:"Win by Silver Goal", abbr:"Win by Silver Goal"},
		// "fifa.winningReason_9":{text:"Win on Golden Goal", abbr:"Win by Golden Goal"},

		if (gameJson.hasOwnProperty('reasonWinCode')) {
			if (gameJson.reasonWinCode === 3) {
				processedMatch["team1PenScore"] = gameJson.scorePenaltyHome
				processedMatch["team2PenScore"] = gameJson.scorePenaltyAway
				processedMatch["statText"] = "Win on penalty"
				processedMatch["resText"] = `${gameJson.scoreHome}-${gameJson.scoreAway} (${gameJson.scorePenaltyHome}-${gameJson.scorePenaltyAway})`				
			}

		} else {
			processedMatch["statText"] = ""
			processedMatch["resText"] = `${gameJson.scoreHome}-${gameJson.scoreAway}`
		}

		processedMatches.push(processedMatch)
	}
}

//main
let path = './raw/matches/';
let files = fs.readdirSync(path);

files.forEach(function(file) {
    processMatchFile(`./raw/matches/${file}`);
});

mkdirp.sync('./processed/matches/');
fs.writeFileSync('./processed/matches/matches.json', JSON.stringify(processedMatches));
console.log('done.')
