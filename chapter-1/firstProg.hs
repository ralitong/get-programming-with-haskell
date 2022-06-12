main :: IO ()
main = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the title"
    title <- getLine
    print "Who is the author?"
    author <- getLine
    print (createEmail recipient title author)

toPart recipient = "Hi " ++ recipient ++ "\n"
bodyPart bookTitle = "Thanks for purchasing " ++ bookTitle ++ ".\n"
fromPart author = "Truly yours,\n" ++ author

createEmail recipient bookTitle author = toPart recipient ++
                                        bodyPart bookTitle ++
                                        fromPart author