# simulate the FIFA World cup 10000 times.

from collections import namedtuple
from itertools import chain, groupby
import csv
import numpy

Match = namedtuple("Match", ['gameId', 'date', 'time', 'team1', 'team2', 'group'])
Prediction = namedtuple("Prediction", ['team1', 'team2', 'outcome', 'sd'])
Team = namedtuple("Team", ['name', 'draw'])
Result = namedtuple("Result", ['match', 'team1Score', 'team2Score'])
TeamResult = namedtuple("TeamResult", ['name', 'opponent', 'group', 'gs', 'ga'])
TeamRecord = namedtuple("TeamRecord", ['name', 'group', 'p', 'gs', 'ga', 'defeated'])

def loadCsv(filepath, itemtype):
    with open(filepath, 'r') as csvfile:
        reader = csv.DictReader(csvfile, delimiter=',')
        return list(map(lambda row: itemtype(**row), reader))

def getActualGroupMatch(match, teams):
    return Match(gameId = match.gameId, date = match.date, time = match.time, group = match.group,
        team1 = next(team.name for team in teams if team.draw == match.team1),
        team2 = next(team.name for team in teams if team.draw == match.team2),       
        )

def getActualEliminationMatch(match, qualified):
    return Match(gameId = match.gameId, date = match.date, time = match.time, group = match.group,
        team1 = qualified[match.team1],
        team2 = qualified[match.team2],
        )

def getResult(match, predictions):
    allowtie = match.group != None 
    prediction = next(pred for pred in predictions \
        if (pred.team1 == match.team1 and pred.team2 == match.team2) or \
            (pred.team1 == match.team2 and pred.team2 == match.team1)) 
    reverse = prediction.team1 != match.team1
    goaldiff = numpy.random.normal(float(prediction.outcome), float(prediction.sd))
    reverse = reverse != (goaldiff < 0) 
    normgoaldiff = 0 if abs(goaldiff) <= 0.4475 \
        else 1 if abs(goaldiff) < 1 else round(abs(goaldiff))
    if not(allowtie) and normgoaldiff == 0:
        normgoaldiff = 1
    losergoals = round(abs(numpy.random.normal(0, 1)))
    return Result(match, losergoals, losergoals + normgoaldiff) if reverse \
        else Result(match, losergoals + normgoaldiff, losergoals)

def getTeamPerfFromResult(result):
    return [TeamResult(name=result.match.team1, opponent=result.match.team2, group=result.match.group, gs=result.team1Score, ga=result.team2Score),
    TeamResult(name=result.match.team2, opponent=result.match.team1, group=result.match.group, gs=result.team2Score, ga=result.team1Score)]

def getTeamRecord(name, teamResults):
    points = w = d = gs = ga = 0
    group = ''
    defeated = []
    for result in teamResults:
        group = result.group
        gs += result.gs
        ga += result.ga
        if result.gs > result.ga:
            w += 1
            defeated.append(result.opponent)
        elif result.gs == result.ga:
            d += 1
    return TeamRecord(name=name, group=group, p=3*w+d, gs=gs, ga=ga, defeated=defeated)

def runGroupStage(schedule, predictions, teams):
    groupschedule = list(map(lambda x: getActualGroupMatch(x, teams), filter(lambda x: x.group != None, schedule)))
    groupresults = list(map(lambda x: getResult(x, predictions), groupschedule))
    groupperfs = list(chain.from_iterable(map(getTeamPerfFromResult, groupresults)))
    records = []
    for k, g in groupby(sorted(groupperfs, key=lambda x: x.name), key=lambda x: x.name):
        records.append(getTeamRecord(k, g))
    qualified = {}
    for k, g in groupby(sorted(records, key=lambda x: x.group), key=lambda x: x.group):
        grouptable = sorted(g, key=lambda x: 1000000*x.p + 10000*(x.gs-x.ga) + 100*(x.gs), reverse=True)
        qualified['1' + k] = grouptable[0].name
        qualified['2' + k] = grouptable[1].name
    return qualified

def runEliminationRound(round, prevround, schedule, predictions):
    roundschedule = list(map(lambda x: getActualEliminationMatch(x, prevround), filter(lambda x: x.gameId.startswith('G'+round), schedule)))
    roundresults = list(map(lambda x: getResult(x, predictions), roundschedule))
    results = {}
    for result in roundresults:
        results['W' + result.match.gameId] = result.match.team1 if result.team1Score > result.team2Score \
            else result.match.team2
        results['L' + result.match.gameId] = result.match.team1 if result.team1Score < result.team2Score \
            else result.match.team2
    return results

#main
schedule = loadCsv('wc2018schedule.csv', Match)
predictions = loadCsv('wc2018staticPredictions.csv', Prediction)
teams = loadCsv('wc2018qualified.csv', Team)
f = open("simresults.csv", "w+")
f.write('iteration,winner\n')
for i in range(10000):
    results = runGroupStage(schedule, predictions, teams)
    for rnd in ['AF','QF','SF','FN']:
        results = runEliminationRound(rnd, results, schedule, predictions)

    f.write(str(i+1) + ',' + results['WGFN2'] + '\n')
