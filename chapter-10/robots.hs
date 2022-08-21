main = do
    print(getAttack killerRobot)
    print(getHp killerRobot)
    print(printRobot killerRobot)
    print(printRobot nicerRobot)
    print(printRobot gentlerRobot)
    print(printRobot softerRobot)
    print(getHp afterHit)
    print(printRobot gentleGiant)
    print(printRobot killerRobot)
    print(printRobot gentleGiantRound1)
    print(printRobot killerRobotRound1)
    print(printRobot gentleGiantRound2)
    print(printRobot killerRobotRound2)
    print(printRobot gentleGiantRound3)
    print(printRobot killerRobotRound3)
    print(printRobot fastRobotRound3)
    print(printRobot slowRobotRound3)
    print(map getStatus [killerRobotRound1, killerRobotRound2, killerRobotRound3])
    print robotsAfterBattle

robot (name, attack, hp) = \message -> message (name, attack, hp)
killerRobot = robot ("Kill3r", 25, 200)
nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHp killerRobot 50
gentleGiant = robot ("Mr. Friendly", 10, 300)
strikeFreedom = robot("X20A Strike Freedom", 15, 250)
infiniteJustice = robot("XGMF Infinite Justice", 18, 300)
name (n,_,_) = n
attack (_, a,_) = a
hp (_,_,hp) = hp
status (n,_,hp) = show n ++ ": " ++ show hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp
getStatus aRobot = aRobot status
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot(\(n, a, h) -> robot (n, newAttack, h))
setHp aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))
printRobot aRobot = aRobot (\(n, a, h) -> n ++
                                            " attack:" ++ (show a) ++
                                            " hp: " ++ (show h))
damage aRobot attackDamage = aRobot (\(n, a, h) ->
                                            robot (n, a, h-attackDamage))
fight aRobot defender = damage defender attack
                where attack = if getHp aRobot > 10
                               then getAttack aRobot
                               else 0



afterHit = damage killerRobot 90
gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2
fastRobot = robot("speedy", 15, 40)
slowRobot = robot("slowpoke", 20, 30)

-- haskell does not care about the arrangement
fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobot fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1


-- 3 round fight is too difficult to implement


participants = [killerRobot, gentlerRobot, strikeFreedom, infiniteJustice]
fightInfiniteJustice = fight (participants !! 3)
robotsAfterBattle = map (getStatus . fightInfiniteJustice) (take (length participants - 1) participants)








