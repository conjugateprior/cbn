WEAT1 <- WEAT2 <- WEAT3 <- WEAT4 <- WEAT5 <- WEAT6 <- WEAT7 <- WEAT8 <- WEAT9 <- WEAT10 <- WEFAT1 <- WEFAT2 <- list()

WEAT1$Flowers <- c("aster", "clover", "hyacinth", "marigold", "poppy", "azalea",
                   "crocus", "iris", "orchid", "rose", "bluebell", "daffodil",
                   "lilac", "pansy", "tulip", "buttercup", "daisy", "lily",
                   "peony", "violet", "carnation", "gladiola", "magnolia",
                   "petunia", "zinnia")
WEAT1$Insects <- c("ant", "caterpillar", "flea", "locust", "spider", "bedbug",
                  "centipede", "fly", "maggot", "tarantula", "bee", "cockroach",
                  "gnat", "mosquito", "termite", "beetle", "cricket", "hornet",
                  "moth", "wasp", "blackfly", "dragonfly", "horsefly", "roach",
                  "weevil")
WEAT1$Pleasant <- c("caress", "freedom", "health", "love", "peace", "cheer",
                    "friend", "heaven", "loyal", "pleasure", "diamond", "gentle",
                    "honest", "lucky", "rainbow", "diploma", "gift", "honor",
                    "miracle", "sunrise", "family", "happy", "laughter",
                    "paradise", "vacation")
WEAT1$Unpleasant <- c("abuse", "crash", "filth", "murder", "sickness",
                      "accident", "death", "grief", "poison", "stink", "assault",
                      "disaster", "hatred", "pollute", "tragedy", "divorce",
                      "jail", "poverty", "ugly", "cancer", "kill", "rotten",
                      "vomit", "agony", "prison")

WEAT2$Instruments <- c("bagpipe", "cello", "guitar", "lute", "trombone", "banjo",
                       "clarinet", "harmonica", "mandolin", "trumpet", "bassoon",
                       "drum", "harp", "oboe", "tuba", "bell", "fiddle",
                       "harpsichord", "piano", "viola", "bongo", "flute", "horn",
                       "saxophone", "violin")
WEAT2$Weapons <- c("arrow", "club", "gun", "missile", "spear", "axe", "dagger",
                   "harpoon", "pistol", "sword", "blade", "dynamite", "hatchet",
                   "rifle", "tank", "bomb", "firearm", "knife", "shotgun",
                   "teargas", "cannon", "grenade", "mace", "slingshot", "whip")
WEAT2$Pleasant <- c("caress", "freedom", "health", "love", "peace", "cheer",
                    "friend", "heaven", "loyal", "pleasure", "diamond", "gentle",
                    "honest", "lucky", "rainbow", "diploma", "gift", "honor",
                    "miracle", "sunrise", "family", "happy", "laughter",
                    "paradise", "vacation")
WEAT2$Unpleasant <- c("abuse", "crash", "filth", "murder", "sickness",
                      "accident", "death", "grief", "poison", "stink", "assault",
                      "disaster", "hatred", "pollute", "tragedy", "divorce",
                      "jail", "poverty", "ugly", "cancer", "kill", "rotten",
                      "vomit", "agony", "prison")

WEAT3$EuropeanAmericanNames_all <- c("Adam", "Chip", "Harry", "Josh", "Roger",
                                 "Alan", "Frank", "Ian", "Justin",
                                 "Ryan", "Andrew", "Fred", "Jack", "Matthew",                                       "Stephen", "Brad", "Greg", "Jed", "Paul",
                                 "Todd", "Brandon", "Hank", "Jonathan", "Peter",                                    "Wilbur", "Amanda", "Courtney", "Heather",
                                 "Melanie", "Sara", "Amber", "Crystal",
                                 "Katie", "Meredith", "Shannon", "Betsy",
                                 "Donna", "Kristin", "Nancy", "Stephanie",
                                 "Bobbie-Sue", "Ellen", "Lauren", "Peggy",
                                 "Sue-Ellen", "Colleen", "Emily", "Megan",
                                 "Rachel", "Wendy")
WEAT3$EuropeanAmericanNames <- c("Adam", "Harry", "Josh", "Roger",
                                     "Alan", "Frank", "Justin",
                                     "Ryan", "Andrew", "Jack", "Matthew",                                              "Stephen", "Brad", "Greg", "Paul",
                                     "Jonathan", "Peter", "Amanda", "Courtney",
                                     "Heather", "Melanie", "Katie", "Betsy",
                                     "Kristin", "Nancy", "Stephanie",
                                     "Ellen", "Lauren", "Colleen", "Emily",
                                     "Megan", "Rachel")
WEAT3$AfricanAmericanNames_all <- c("Alonzo", "Jamel", "Lerone", "Percell",
                                    "Theo", "Alphonse", "Jerome", "Leroy",
                                    "Rasaan", "Torrance", "Darnell", "Lamar",
                                    "Lionel", "Rashaun", "Tyree", "Deion",
                                    "Lamont", "Malik", "Terrence", "Tyrone",
                                    "Everol", "Lavon", "Marcellus", "Terryl",
                                    "Wardell", "Aiesha", "Lashelle", "Nichelle",
                                    "Shereen", "Temeka", "Ebony", "Latisha",                                          "Shaniqua", "Tameisha", "Teretha", "Jasmine",                                     "Latonya", "Shanise", "Tanisha", "Tia",
                                    "Lakisha", "Latoya", "Sharise", "Tashika",                                        "Yolanda", "Lashandra", "Malika", "Shavonn",
                                    "Tawanda", "Yvette")
WEAT3$AfricanAmericanNames <- c("Alonzo", "Jamel",
                                "Theo", "Alphonse", "Jerome", "Leroy",
                                "Torrance", "Darnell", "Lamar",
                                "Lionel", "Tyree", "Deion",
                                "Lamont", "Malik", "Terrence", "Tyrone",
                                "Lavon", "Marcellus",
                                "Wardell", "Nichelle",
                                "Shereen", "Ebony", "Latisha",                                                    "Shaniqua", "Jasmine", "Tanisha", "Tia",
                                "Lakisha", "Latoya", "Yolanda", "Malika",
                                "Yvette")
# as above
WEAT3$Pleasant <- c("caress", "freedom", "health", "love", "peace", "cheer",
               "friend", "heaven", "loyal", "pleasure", "diamond", "gentle",
               "honest", "lucky", "rainbow", "diploma", "gift", "honor",
               "miracle", "sunrise", "family", "happy", "laughter",
               "paradise", "vacation")
# not quite as above
WEAT3$Unpleasant <- c("abuse", "crash", "filth", "murder", "sickness",
                       "accident", "death", "grief", "poison", "stink", "assault",
                       "disaster", "hatred", "pollute", "tragedy", "divorce",
                       "bomb", #added
                       "jail", "poverty", "ugly", "cancer", "evil", "kill", "rotten",
                       "vomit"
                       # "agony", "prison" # subtracted
                       )

WEAT4$EuropeanAmericanNames_all <- c("Brad", "Brendan", "Geoffrey", "Greg",
                                     "Brett", "Jay", "Matthew", "Neil",
                                     "Todd", "Allison", "Anne", "Carrie",
                                     "Emily", "Jill", "Laurie", "Kristen",
                                     "Meredith", "Sarah")
WEAT4$EuropeanAmericanNames <- c("Brad", "Brendan", "Geoffrey", "Greg",
                                     "Brett", "Matthew", "Neil",
                                     "Todd", "Allison", "Anne", "Carrie",
                                     "Emily", "Jill", "Laurie",
                                     "Meredith", "Sarah")
WEAT4$AfricanAmericanNames_all <- c("Darnell", "Hakim", "Jermaine", "Kareem",
                                    "Jamal", "Leroy", "Rasheed", "Tremayne",
                                    "Tyrone", "Aisha", "Ebony", "Keisha",
                                    "Kenya", "Latonya", "Lakisha", "Latoya",
                                    "Tamika", "Tanisha")
WEAT4$AfricanAmericanNames <- c("Darnell", "Hakim", "Jermaine", "Kareem",
                                "Jamal", "Leroy", "Rasheed",
                                "Tyrone", "Aisha", "Ebony", "Keisha",
                                "Kenya", "Lakisha", "Latoya",
                                "Tamika", "Tanisha")
WEAT4$Pleasant <- c("caress", "freedom", "health", "love", "peace", "cheer",
                    "friend", "heaven", "loyal", "pleasure", "diamond", "gentle",
                    "honest", "lucky", "rainbow", "diploma", "gift", "honor",
                    "miracle", "sunrise", "family", "happy", "laughter",
                    "paradise", "vacation")
WEAT4$Unpleasant <- c("abuse", "crash", "filth", "murder", "sickness",
                      "accident", "death", "grief", "poison", "stink", "assault",
                      "disaster", "hatred", "pollute", "tragedy", "divorce",
                      "bomb", #added
                      "jail", "poverty", "ugly", "cancer", "evil", "kill", "rotten",
                      "vomit"
                      # "agony", "prison" # subtracted
                      )
WEAT5$EuropeanAmericanNames_all <- c("Brad", "Brendan", "Geoffrey", "Greg",
                                     "Brett", "Jay", "Matthew", "Neil",
                                     "Todd", "Allison", "Anne", "Carrie",
                                     "Emily", "Jill", "Laurie", "Kristen",
                                     "Meredith", "Sarah")
WEAT5$EuropeanAmericanNames <- c("Brad", "Brendan", "Geoffrey", "Greg",
                                 "Brett", "Matthew", "Neil",
                                 "Todd", "Allison", "Anne", "Carrie",
                                 "Emily", "Jill", "Laurie",
                                 "Meredith", "Sarah")
WEAT5$AfricanAmericanNames_all <- c("Darnell", "Hakim", "Jermaine", "Kareem",
                                    "Jamal", "Leroy", "Rasheed", "Tremayne",
                                    "Tyrone", "Aisha", "Ebony", "Keisha", "Kenya",
                                    "Latonya", "Lakisha", "Latoya", "Tamika",
                                    "Tanisha")
WEAT5$AfricanAmericanNames <- c("Darnell", "Hakim", "Jermaine", "Kareem",
                                "Jamal", "Leroy", "Rasheed",
                                "Tyrone", "Aisha", "Ebony", "Keisha", "Kenya",
                                "Lakisha", "Latoya", "Tamika",
                                "Tanisha")
WEAT5$Pleasant <- c("joy", "love", "peace", "wonderful", "pleasure", "friend",
                   "laughter", "happy")
WEAT5$Unpleasant <- c("agony", "terrible", "horrible", "nasty", "evil", "war",
                      "awful", "failure")
WEAT6$MaleNames <- c("John", "Paul", "Mike", "Kevin", "Steve", "Greg", "Jeff",
                     "Bill")
WEAT6$FemaleNames <- c("Amy", "Joan", "Lisa", "Sarah", "Diana", "Kate", "Ann",
                       "Donna")
WEAT6$Career <- c("executive", "management", "professional", "corporation",
                  "salary", "office", "business", "career")
WEAT6$Family <- c("home", "parents", "children", "family", "cousins", "marriage",
                  "wedding", "relatives")
WEAT7$Math <- c("math", "algebra", "geometry", "calculus", "equations",
                "computation", "numbers", "addition")
WEAT7$Arts <- c("poetry", "art", "dance", "literature", "novel", "symphony",
                "drama", "sculpture")
WEAT7$MaleTerms <- c("male", "man", "boy", "brother", "he", "him", "his", "son")
WEAT7$FemaleTerms <- c("female", "woman", "girl", "sister", "she", "her",
                       "hers", "daughter")
WEAT8$Science <- c("science", "technology", "physics", "chemistry", "Einstein",
                   "NASA", "experiment", "astronomy")
WEAT8$Arts <- c("poetry", "art", "Shakespeare", "dance", "literature", "novel",
                "symphony", "drama")
WEAT8$MaleTerms <- c("brother", "father", "uncle", "grandfather", "son", "he",
                     "his", "him")
WEAT8$FemaleTerms <- c("sister", "mother", "aunt", "grandmother", "daughter",
                       "she", "hers", "her")

WEAT9$MentalDisease <- c("sad", "hopeless", "gloomy", "tearful", "miserable",
                         "depressed")
WEAT9$PhysicalDisease <- c("sick", "illness", "influenza", "disease", "virus",
                           "cancer")
WEAT9$Temporary <- c("impermanent", "unstable", "variable", "fleeting",
                     "short-term", "brief", "occasional") # w2v used 'short'
WEAT9$Permanent <- c("stable", "always", "constant", "persistent", "chronic",
                     "prolonged", "forever")
WEAT10$YoungNames <- c("Tiffany", "Michelle", "Cindy", "Kristy", "Brad",
                       "Eric", "Joey", "Billy")
WEAT10$OldNames <- c("Ethel", "Bernice", "Gertrude", "Agnes", "Cecil",
                     "Wilbert", "Mortimer", "Edgar")
WEAT10$Pleasant <- c("joy", "love", "peace", "wonderful", "pleasure", "friend",
                     "laughter", "happy")
WEAT10$Unpleasant <- c("agony", "terrible", "horrible", "nasty", "evil", "war",
                       "awful", "failure")
WEFAT1$Careers <- c("technician", "accountant", "supervisor", "engineer",
                    "worker", "educator", "clerk", "counselor", "inspector",
                    "mechanic", "manager", "therapist", "administrator",
                    "salesperson", "receptionist", "librarian", "advisor",
                    "pharmacist", "janitor", "psychologist", "physician",
                    "carpenter", "nurse", "investigator", "bartender",
                    "specialist", "electrician", "officer", "pathologist",
                    "teacher", "lawyer", "planner", "practitioner", "plumber",
                    "instructor", "surgeon", "veterinarian", "paramedic",
                    "examiner", "chemist", "machinist", "appraiser",
                    "nutritionist", "architect", "hairdresser", "baker",
                    "programmer", "paralegal", "hygienist", "scientist")
WEFAT1$FemaleAttributes <- c("female", "woman", "girl", "sister", "she", "her",
                             "hers", "daughter")
WEFAT1$MaleAttributes <- c("male", "man", "boy", "brother", "he", "him", "his",
                           "son")
WEFAT2$AndrogynousNames <- c("Kelly", "Tracy", "Jamie", "Jackie", "Jesse",
                             "Courtney", "Lynn", "Taylor", "Leslie", "Shannon",
                             "Stacey", "Jessie", "Shawn", "Stacy", "Casey",
                             "Bobby", "Terry", "Lee", "Ashley", "Eddie",
                             "Chris", "Jody", "Pat", "Carey", "Willie", "Morgan",
                             "Robbie", "Joan", "Alexis", "Kris", "Frankie",
                             "Bobbie", "Dale", "Robin", "Billie", "Adrian",
                             "Kim", "Jaime", "Jean", "Francis", "Marion",
                             "Dana", "Rene", "Johnnie", "Jordan", "Carmen",
                             "Ollie", "Dominique", "Jimmie", "Shelby")
WEFAT2$FemaleAttributes <- c("female", "woman", "girl", "sister", "she", "her",
                             "hers", "daughter")
WEFAT2$MaleAttributes <- c("male", "man", "boy", "brother", "he", "him", "his",
                           "son")

unused_vocab <- c("Chip", "Fred", "Jed", "Todd", "Brandon", "Hank", "Wilbur",
                  "Ian", "Sara", "Amber", "Crystal", "Meredith", "Shannon",
                  "Betsy", "Donna", "Bobbie-Sue", "Peggy", "Sue-Ellen", "Wendy",
                  "Lerone", "Percell", "Rasaan", "Rashaun", "Everol", "Terryl",
                  "Aiesha", "Lashelle", "Temeka", "Tameisha", "Tereth",
                  "Latonya", "Shanise", "Sharise", "Tashika", "Lashandra",
                  "Shavonn", "Tawanda", "Jay", "Kristen", "Tremayne")

download_common_crawl <- function(folder = "."){
  message("This might take a few minutes...")
  download.file("http://nlp.stanford.edu/data/glove.840B.300d.zip",
                destfile = file.path(folder, "glove.840B.300d.zip"))
  message("Download complete.  Unzipping")
  unzip(file.path(folder, "glove.840B.300d.zip"))
  message("The unzipped file should be: ",
          file.path(folder, "glove.840B.300d.txt"))
}

