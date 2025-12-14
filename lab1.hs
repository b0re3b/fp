{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where
import Control.Monad (void, when, forM_)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Control.Monad (void, when)
import Control.Exception (catch, SomeException)
import Text.Printf (printf)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- ERROR TYPES
data ServiceError
    = ValidationError String
    | DatabaseError String
    | NotFoundError String
    | ConnectionError String
    deriving (Show, Eq)

-- TYPECLASSES
class Storable a where
    insert :: Connection -> a -> IO (Either ServiceError Int)
    update :: Connection -> a -> IO (Either ServiceError ())
    delete :: Connection -> Int -> IO (Either ServiceError ())
    findById :: Connection -> Int -> IO (Either ServiceError (Maybe a))
    findAll :: Connection -> IO (Either ServiceError [a])

class Validatable a where
    validate :: a -> Either ServiceError a

class Displayable a where
    display :: a -> String
    displayTable :: [a] -> String

-- DATA TYPES
data Service = Service
    { serviceId :: Maybe Int
    , serviceName :: Text
    , serviceType :: Text
    , version :: Text
    , description :: Text
    , deadline :: Maybe Day
    , createdAt :: Maybe UTCTime
    } deriving (Show, Generic)

instance FromRow Service where
    fromRow = Service <$> field <*> field <*> field <*> field
                      <*> field <*> field <*> field

instance ToRow Service where
    toRow (Service _ name stype ver desc ddl _) =
        toRow (name, stype, ver, desc, ddl)

data Author = Author
    { authorId :: Maybe Int
    , authorName :: Text
    , authorEmail :: Text
    , authorDepartment :: Text
    , authorPosition :: Text
    , authorPhone :: Text
    } deriving (Show, Generic)

instance FromRow Author where
    fromRow = Author <$> field <*> field <*> field <*> field
                     <*> field <*> field

instance ToRow Author where
    toRow (Author _ name email dept pos phone) =
        toRow (name, email, dept, pos, phone)

data ServiceAuthor = ServiceAuthor
    { saId :: Maybe Int
    , saServiceId :: Int
    , saAuthorId :: Int
    } deriving (Show, Generic)

instance FromRow ServiceAuthor where
    fromRow = ServiceAuthor <$> field <*> field <*> field

instance ToRow ServiceAuthor where
    toRow (ServiceAuthor _ sid aid) = toRow (sid, aid)

data UsageTerms = UsageTerms
    { termsId :: Maybe Int
    , termsServiceId :: Int
    , termsText :: Text
    , licenseType :: Text
    , restrictions :: Text
    } deriving (Show, Generic)

instance FromRow UsageTerms where
    fromRow = UsageTerms <$> field <*> field <*> field <*> field <*> field

instance ToRow UsageTerms where
    toRow (UsageTerms _ sid txt lic restr) =
        toRow (sid, txt, lic, restr)

data User = User
    { userId :: Maybe Int
    , userName :: Text
    , userEmail :: Text
    , userPhone :: Text
    , userAffiliation :: Text
    , registrationDate :: Maybe UTCTime
    , userStatus :: Text
    } deriving (Show, Generic)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field
                   <*> field <*> field <*> field

instance ToRow User where
    toRow (User _ name email phone aff _ status) =
        toRow (name, email, phone, aff, status)

data UsageStatistics = UsageStatistics
    { statId :: Maybe Int
    , statServiceId :: Int
    , statUserId :: Int
    , accessDate :: Maybe UTCTime
    , duration :: Int
    , actionType :: Text
    } deriving (Show, Generic)

instance FromRow UsageStatistics where
    fromRow = UsageStatistics <$> field <*> field <*> field
                              <*> field <*> field <*> field

instance ToRow UsageStatistics where
    toRow (UsageStatistics _ sid uid _ dur act) =
        toRow (sid, uid, dur, act)

data ServiceReport = ServiceReport
    { reportServiceName :: Text
    , reportAuthorCount :: Int
    , reportUsageCount :: Int
    , reportTotalDuration :: Int
    , reportLastAccess :: Maybe UTCTime
    } deriving (Show)

-- VALIDATION FUNCTIONS
isValidEmail :: Text -> Bool
isValidEmail email =
    let parts = T.split (== '@') email
    in case parts of
        [user, domain] ->
            not (T.null user) &&
            not (T.null domain) &&
            T.isInfixOf "." domain
        _ -> False

isValidVersion :: Text -> Bool
isValidVersion v =
    let parts = T.split (== '.') v
    in length parts >= 2 && all (not . T.null) parts

isValidPhone :: Text -> Bool
isValidPhone phone =
    let digits = T.filter (`elem` "0123456789+-() ") phone
    in T.length (T.filter (`elem` "0123456789") digits) >= 10

instance Validatable Service where
    validate s
        | T.null (serviceName s) =
            Left (ValidationError "Service name cannot be empty")
        | T.null (serviceType s) =
            Left (ValidationError "Service type cannot be empty")
        | T.null (version s) =
            Left (ValidationError "Version cannot be empty")
        | not (isValidVersion (version s)) =
            Left (ValidationError "Invalid version format (expected X.Y.Z)")
        | T.length (description s) < 10 =
            Left (ValidationError "Description must be at least 10 characters")
        | otherwise = Right s

instance Validatable Author where
    validate a
        | T.null (authorName a) =
            Left (ValidationError "Author name cannot be empty")
        | not (isValidEmail (authorEmail a)) =
            Left (ValidationError "Invalid email")
        | T.null (authorDepartment a) =
            Left (ValidationError "Department cannot be empty")
        | not (isValidPhone (authorPhone a)) =
            Left (ValidationError "Invalid phone number")
        | otherwise = Right a

instance Validatable User where
    validate u
        | T.null (userName u) =
            Left (ValidationError "Username cannot be empty")
        | not (isValidEmail (userEmail u)) =
            Left (ValidationError "Invalid email")
        | T.length (userEmail u) > 255 =
            Left (ValidationError "Email too long")
        | otherwise = Right u

instance Validatable UsageTerms where
    validate t
        | T.null (termsText t) =
            Left (ValidationError "Terms text cannot be empty")
        | T.null (licenseType t) =
            Left (ValidationError "License type cannot be empty")
        | otherwise = Right t

-- DISPLAY INSTANCES
instance Displayable Service where
    display s = unlines
        [ "====== SERVICE INFO ======"
        , printf "ID: %s" (maybe "--" show (serviceId s))
        , printf "Name: %s" (T.unpack (serviceName s))
        , printf "Type: %s" (T.unpack (serviceType s))
        , printf "Version: %s" (T.unpack (version s))
        , printf "Description: %s" (T.unpack (description s))
        , printf "Deadline: %s" (maybe "Not set" show (deadline s))
        , printf "Created: %s" (maybe "--" show (createdAt s))
        ]

    displayTable [] = "No services to display"
    displayTable services = unlines
        ( [ "ID  | Name                | Type      | Version"
          ] ++ map formatRow services
        )
        where
            formatRow s = printf "%-3s | %-19s | %-9s | %-7s"
                (maybe "--" show (serviceId s))
                (T.unpack $ T.take 19 (serviceName s))
                (T.unpack $ T.take 9 (serviceType s))
                (T.unpack $ T.take 7 (version s))

instance Displayable Author where
    display a = unlines
        [ "====== AUTHOR INFO ======"
        , printf "ID: %s" (maybe "--" show (authorId a))
        , printf "Name: %s" (T.unpack (authorName a))
        , printf "Email: %s" (T.unpack (authorEmail a))
        , printf "Department: %s" (T.unpack (authorDepartment a))
        , printf "Position: %s" (T.unpack (authorPosition a))
        , printf "Phone: %s" (T.unpack (authorPhone a))
        ]

    displayTable [] = "No authors to display"
    displayTable authors = unlines
        ( [ "ID  | Name                | Email               | Department"
          ] ++ map formatRow authors
        )
        where
            formatRow a = printf "%-3s | %-19s | %-19s | %-15s"
                (maybe "--" show (authorId a))
                (T.unpack $ T.take 19 (authorName a))
                (T.unpack $ T.take 19 (authorEmail a))
                (T.unpack $ T.take 15 (authorDepartment a))

instance Displayable User where
    display u = unlines
        [ "====== USER INFO ======"
        , printf "ID: %s" (maybe "--" show (userId u))
        , printf "Name: %s" (T.unpack (userName u))
        , printf "Email: %s" (T.unpack (userEmail u))
        , printf "Phone: %s" (T.unpack (userPhone u))
        , printf "Affiliation: %s" (T.unpack (userAffiliation u))
        , printf "Status: %s" (T.unpack (userStatus u))
        , printf "Registered: %s" (maybe "--" show (registrationDate u))
        ]

    displayTable [] = "No users to display"
    displayTable users = unlines
        ( [ "ID  | Name                | Email              | Status"
          ] ++ map formatRow users
        )
        where
            formatRow u = printf "%-3s | %-19s | %-18s | %-8s"
                (maybe "--" show (userId u))
                (T.unpack $ T.take 19 (userName u))
                (T.unpack $ T.take 18 (userEmail u))
                (T.unpack $ T.take 8 (userStatus u))

instance Displayable ServiceReport where
    display r = unlines
        [ "====== SERVICE REPORT ======"
        , printf "Service: %s" (T.unpack (reportServiceName r))
        , printf "Author Count: %d" (reportAuthorCount r)
        , printf "Usage Count: %d" (reportUsageCount r)
        , printf "Total Duration: %d minutes" (reportTotalDuration r)
        , printf "Last Access: %s" (maybe "--" show (reportLastAccess r))
        ]

    displayTable [] = "No reports"
    displayTable reports = unlines
        ( [ "Service             | Authors | Usage | Duration"
          ] ++ map formatRow reports
        )
        where
            formatRow r = printf "%-19s | %-7d | %-5d | %-8d"
                (T.unpack $ T.take 19 (reportServiceName r))
                (reportAuthorCount r)
                (reportUsageCount r)
                (reportTotalDuration r)

-- DATABASE OPERATIONS
instance Storable Service where
    insert conn service = do
        case validate service of
            Left err -> return $ Left err
            Right validService ->
                (do
                    execute conn
                        "INSERT INTO services (name, type, version, description, deadline) VALUES (?, ?, ?, ?, ?)"
                        validService
                    return $ Right 0
                ) `catch` \(e :: SomeException) ->
                    return $ Left (DatabaseError $ show e)

    update conn service = do
        case validate service of
            Left err -> return $ Left err
            Right validService ->
                (do
                    execute conn
                        "UPDATE services SET name=?, type=?, version=?, description=?, deadline=? WHERE id=?"
                        (serviceName validService, serviceType validService, version validService,
                         description validService, deadline validService, serviceId validService)
                    return $ Right ()
                ) `catch` \(e :: SomeException) ->
                    return $ Left (DatabaseError $ show e)

    delete conn sid =
        (do
            execute conn "DELETE FROM services WHERE id=?" (Only sid)
            return $ Right ()
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    findById conn sid =
        (do
            result <- (query conn "SELECT * FROM services WHERE id=?" (Only sid)) :: IO [Service]
            return $ Right $ case result of
                [] -> Nothing
                (s:_) -> Just s
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    findAll conn =
        (do
            result <- (query_ conn "SELECT * FROM services") :: IO [Service]
            return $ Right result
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

instance Storable Author where
    insert conn author = do
        case validate author of
            Left err -> return $ Left err
            Right validAuthor ->
                (do
                    execute conn
                        "INSERT INTO authors (name, email, department, position, phone) VALUES (?, ?, ?, ?, ?)"
                        validAuthor
                    return $ Right 0
                ) `catch` \(e :: SomeException) ->
                    return $ Left (DatabaseError $ show e)

    update conn author = do
        case validate author of
            Left err -> return $ Left err
            Right validAuthor ->
                (do
                    execute conn
                        "UPDATE authors SET name=?, email=?, department=?, position=?, phone=? WHERE id=?"
                        (authorName validAuthor, authorEmail validAuthor, authorDepartment validAuthor,
                         authorPosition validAuthor, authorPhone validAuthor, authorId validAuthor)
                    return $ Right ()
                ) `catch` \(e :: SomeException) ->
                    return $ Left (DatabaseError $ show e)

    delete conn aid =
        (do
            execute conn "DELETE FROM authors WHERE id=?" (Only aid)
            return $ Right ()
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    findById conn aid =
        (do
            result <- (query conn "SELECT * FROM authors WHERE id=?" (Only aid)) :: IO [Author]
            return $ Right $ case result of
                [] -> Nothing
                (a:_) -> Just a
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    findAll conn =
        (do
            result <- (query_ conn "SELECT * FROM authors") :: IO [Author]
            return $ Right result
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

instance Storable User where
    insert conn user = do
        case validate user of
            Left err -> return $ Left err
            Right validUser ->
                (do
                    execute conn
                        "INSERT INTO users (name, email, phone, affiliation, status) VALUES (?, ?, ?, ?, ?)"
                        validUser
                    return $ Right 0
                ) `catch` \(e :: SomeException) ->
                    return $ Left (DatabaseError $ show e)

    update conn user = do
        case validate user of
            Left err -> return $ Left err
            Right validUser ->
                (do
                    execute conn
                        "UPDATE users SET name=?, email=?, phone=?, affiliation=?, status=? WHERE id=?"
                        (userName validUser, userEmail validUser, userPhone validUser,
                         userAffiliation validUser, userStatus validUser, userId validUser)
                    return $ Right ()
                ) `catch` \(e :: SomeException) ->
                    return $ Left (DatabaseError $ show e)

    delete conn uid =
        (do
            execute conn "DELETE FROM users WHERE id=?" (Only uid)
            return $ Right ()
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    findById conn uid =
        (do
            result <- (query conn "SELECT * FROM users WHERE id=?" (Only uid)) :: IO [User]
            return $ Right $ case result of
                [] -> Nothing
                (u:_) -> Just u
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    findAll conn =
        (do
            result <- (query_ conn "SELECT * FROM users") :: IO [User]
            return $ Right result
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

instance Storable UsageStatistics where
    insert conn stat =
        (do
            execute conn
                "INSERT INTO usage_statistics (service_id, user_id, duration, action_type) VALUES (?, ?, ?, ?)"
                stat
            return $ Right 0
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    update conn stat =
        (do
            execute conn
                "UPDATE usage_statistics SET service_id=?, user_id=?, duration=?, action_type=? WHERE id=?"
                (statServiceId stat, statUserId stat, duration stat,
                 actionType stat, statId stat)
            return $ Right ()
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    delete conn sid =
        (do
            execute conn "DELETE FROM usage_statistics WHERE id=?" (Only sid)
            return $ Right ()
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    findById conn sid =
        (do
            result <- query conn "SELECT * FROM usage_statistics WHERE id=?" (Only sid) :: IO [UsageStatistics]
            return $ Right $ case result of
                [] -> Nothing
                (s:_) -> Just s
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

    findAll conn =
        (do
            result <- query_ conn "SELECT * FROM usage_statistics" :: IO [UsageStatistics]
            return $ Right result
        ) `catch` \(e :: SomeException) ->
            return $ Left (DatabaseError $ show e)

-- SPECIALIZED QUERIES
findServicesByType :: Connection -> Text -> IO (Either ServiceError [Service])
findServicesByType conn stype =
    (do
        result <- (query conn "SELECT * FROM services WHERE type=?" (Only stype)) :: IO [Service]
        return $ Right result
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

findServiceAuthors :: Connection -> Int -> IO (Either ServiceError [Author])
findServiceAuthors conn sid =
    (do
        result <- (query conn "SELECT a.* FROM authors a \
                   \JOIN service_authors sa ON a.id = sa.author_id \
                   \WHERE sa.service_id=?" (Only sid)) :: IO [Author]
        return $ Right result
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

getServiceStatistics :: Connection -> Int -> IO (Either ServiceError [UsageStatistics])
getServiceStatistics conn sid =
    (do
        result <- (query conn "SELECT * FROM usage_statistics WHERE service_id=? \
                   \ORDER BY access_date DESC" (Only sid)) :: IO [UsageStatistics]
        return $ Right result
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

getTopServices :: Connection -> Int -> IO (Either ServiceError [(Text, Int)])
getTopServices conn limit =
    (do
        result <- (query conn "SELECT s.name, COUNT(*) as usage_count \
                   \FROM services s \
                   \LEFT JOIN usage_statistics us ON s.id = us.service_id \
                   \GROUP BY s.id, s.name \
                   \ORDER BY usage_count DESC \
                   \LIMIT ?" (Only limit)) :: IO [(Text, Int)]
        return $ Right result
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

getServiceReport :: Connection -> Int -> IO (Either ServiceError (Maybe ServiceReport))
getServiceReport conn sid =
    (do
        services <- (query conn "SELECT * FROM services WHERE id=?" (Only sid)) :: IO [Service]
        case services of
            [] -> return $ Right Nothing
            (s:_) -> do
                authors <- (query conn
                    "SELECT COUNT(*) FROM service_authors WHERE service_id=?"
                    (Only sid)) :: IO [(Only Int)]
                stats <- (query conn
                    "SELECT COUNT(*), SUM(duration), MAX(access_date) FROM usage_statistics WHERE service_id=?"
                    (Only sid)) :: IO [(Maybe Int, Maybe Int, Maybe UTCTime)]

                let authCount = case authors of
                        [(Only c)] -> c
                        _ -> 0
                let (usageCount, totalDur, lastAccess) = case stats of
                        [(c, d, a)] -> (maybe 0 id c, maybe 0 id d, a)
                        _ -> (0, 0, Nothing)

                return $ Right $ Just $ ServiceReport
                    (serviceName s) authCount usageCount totalDur lastAccess
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

getActiveUsers :: Connection -> IO (Either ServiceError [User])
getActiveUsers conn =
    (do
        result <- (query_ conn "SELECT * FROM users WHERE status='active' ORDER BY registration_date DESC") :: IO [User]
        return $ Right result
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

getUserActivity :: Connection -> Int -> IO (Either ServiceError [(Text, Int)])
getUserActivity conn uid =
    (do
        result <- (query conn "SELECT s.name, COUNT(*) as access_count \
                   \FROM services s \
                   \JOIN usage_statistics us ON s.id = us.service_id \
                   \WHERE us.user_id=? \
                   \GROUP BY s.id, s.name \
                   \ORDER BY access_count DESC" (Only uid)) :: IO [(Text, Int)]
        return $ Right result
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

-- DATABASE INITIALIZATION
initDatabase :: Connection -> IO (Either ServiceError ())
initDatabase conn =
    (do
        execute_ conn "CREATE TABLE IF NOT EXISTS services (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \name TEXT NOT NULL UNIQUE, \
            \type TEXT NOT NULL, \
            \version TEXT NOT NULL, \
            \description TEXT NOT NULL, \
            \deadline DATE, \
            \created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"

        execute_ conn "CREATE TABLE IF NOT EXISTS authors (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \name TEXT NOT NULL, \
            \email TEXT NOT NULL UNIQUE, \
            \department TEXT NOT NULL, \
            \position TEXT NOT NULL, \
            \phone TEXT NOT NULL)"

        execute_ conn "CREATE TABLE IF NOT EXISTS service_authors (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \service_id INTEGER NOT NULL, \
            \author_id INTEGER NOT NULL, \
            \FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE, \
            \FOREIGN KEY (author_id) REFERENCES authors(id) ON DELETE CASCADE, \
            \UNIQUE (service_id, author_id))"

        execute_ conn "CREATE TABLE IF NOT EXISTS usage_terms (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \service_id INTEGER NOT NULL, \
            \terms_text TEXT NOT NULL, \
            \license_type TEXT NOT NULL, \
            \restrictions TEXT, \
            \FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE)"

        execute_ conn "CREATE TABLE IF NOT EXISTS users (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \name TEXT NOT NULL, \
            \email TEXT NOT NULL UNIQUE, \
            \phone TEXT, \
            \affiliation TEXT, \
            \registration_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP, \
            \status TEXT DEFAULT 'active')"

        execute_ conn "CREATE TABLE IF NOT EXISTS usage_statistics (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \service_id INTEGER NOT NULL, \
            \user_id INTEGER NOT NULL, \
            \access_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP, \
            \duration INTEGER DEFAULT 0, \
            \action_type TEXT NOT NULL, \
            \FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE, \
            \FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE)"

        return $ Right ()
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

-- POPULATE DATABASE WITH SAMPLE DATA
populateDatabase :: Connection -> IO (Either ServiceError ())
populateDatabase conn =
    (do
        -- Insert sample authors
        execute conn
            "INSERT OR IGNORE INTO authors (name, email, department, position, phone) VALUES (?, ?, ?, ?, ?)"
            (Author Nothing "Dr. Ivan Petrov" "ivan.petrov@university.edu" "Computer Science" "Professor" "+380501234567")

        execute conn
            "INSERT OR IGNORE INTO authors (name, email, department, position, phone) VALUES (?, ?, ?, ?, ?)"
            (Author Nothing "Dr. Maria Sidorenko" "maria.sidorenko@university.edu" "Mathematics" "Associate Professor" "+380509876543")

        execute conn
            "INSERT OR IGNORE INTO authors (name, email, department, position, phone) VALUES (?, ?, ?, ?, ?)"
            (Author Nothing "Dr. Oleksiy Kovalenko" "oleksiy.kovalenko@university.edu" "Physics" "Senior Lecturer" "+380505555555")

        -- Insert sample services
        execute conn
            "INSERT OR IGNORE INTO services (name, type, version, description, deadline) VALUES (?, ?, ?, ?, ?)"
            (Service Nothing "Online Tutoring Platform" "Educational" "1.2.0"
             "Comprehensive online tutoring service for undergraduate and graduate students with real-time video support"
             (Just (fromGregorian 2025 12 31)))

        execute conn
            "INSERT OR IGNORE INTO services (name, type, version, description, deadline) VALUES (?, ?, ?, ?, ?)"
            (Service Nothing "Research Management System" "Research Tools" "2.1.3"
             "Advanced system for managing research projects, publications, and collaboration between faculty members"
             (Just (fromGregorian 2025 6 30)))

        execute conn
            "INSERT OR IGNORE INTO services (name, type, version, description, deadline) VALUES (?, ?, ?, ?, ?)"
            (Service Nothing "Course Content Repository" "Library" "1.0.5"
             "Centralized digital library for course materials, textbooks, and educational resources across all departments"
             Nothing)

        -- Insert sample users
        execute conn
            "INSERT OR IGNORE INTO users (name, email, phone, affiliation, status) VALUES (?, ?, ?, ?, ?)"
            (User Nothing "John Smith" "john.smith@student.edu" "+380501111111" "Student" "active")

        execute conn
            "INSERT OR IGNORE INTO users (name, email, phone, affiliation, status) VALUES (?, ?, ?, ?, ?)"
            (User Nothing "Anna Kowalski" "anna.kowalski@student.edu" "+380502222222" "Student" "active")

        execute conn
            "INSERT OR IGNORE INTO users (name, email, phone, affiliation, status) VALUES (?, ?, ?, ?, ?)"
            (User Nothing "Robert Johnson" "robert.johnson@faculty.edu" "+380503333333" "Faculty" "active")

        -- Insert service-author relationships
        execute conn
            "INSERT OR IGNORE INTO service_authors (service_id, author_id) VALUES (?, ?)"
            (1 :: Int, 1 :: Int)

        execute conn
            "INSERT OR IGNORE INTO service_authors (service_id, author_id) VALUES (?, ?)"
            (1 :: Int, 2 :: Int)

        execute conn
            "INSERT OR IGNORE INTO service_authors (service_id, author_id) VALUES (?, ?)"
            (2 :: Int, 1 :: Int)

        execute conn
            "INSERT OR IGNORE INTO service_authors (service_id, author_id) VALUES (?, ?)"
            (3 :: Int, 3 :: Int)

        -- Insert usage terms
        execute conn
            "INSERT OR IGNORE INTO usage_terms (service_id, terms_text, license_type, restrictions) VALUES (?, ?, ?, ?)"
            (UsageTerms Nothing 1
             "Users must comply with university policies and maintain academic integrity standards"
             "Educational License"
             "For university members only, non-commercial use")

        execute conn
            "INSERT OR IGNORE INTO usage_terms (service_id, terms_text, license_type, restrictions) VALUES (?, ?, ?, ?)"
            (UsageTerms Nothing 2
             "Researchers must acknowledge the university in publications using this system"
             "Academic Research License"
             "Attribution required in all publications")

        -- Insert usage statistics
        execute conn
            "INSERT INTO usage_statistics (service_id, user_id, duration, action_type) VALUES (?, ?, ?, ?)"
            (UsageStatistics Nothing 1 1 120 "login")

        execute conn
            "INSERT INTO usage_statistics (service_id, user_id, duration, action_type) VALUES (?, ?, ?, ?)"
            (UsageStatistics Nothing 1 1 90 "session")

        execute conn
            "INSERT INTO usage_statistics (service_id, user_id, duration, action_type) VALUES (?, ?, ?, ?)"
            (UsageStatistics Nothing 2 2 150 "login")

        execute conn
            "INSERT INTO usage_statistics (service_id, user_id, duration, action_type) VALUES (?, ?, ?, ?)"
            (UsageStatistics Nothing 2 3 45 "query")

        execute conn
            "INSERT INTO usage_statistics (service_id, user_id, duration, action_type) VALUES (?, ?, ?, ?)"
            (UsageStatistics Nothing 3 2 200 "download")

        return $ Right ()
    ) `catch` \(e :: SomeException) ->
        return $ Left (DatabaseError $ show e)

-- DATABASE CONNECTION
connectDB :: IO (Either ServiceError Connection)
connectDB =
    (do
        conn <- open "faculty_services.db"
        return $ Right conn
    ) `catch` \(e :: SomeException) ->
        return $ Left (ConnectionError $ show e)

-- UTILITY FUNCTIONS
printHeader :: String -> IO ()
printHeader title = do
    putStrLn ""
    putStrLn $ replicate 60 '='
    putStrLn title
    putStrLn $ replicate 60 '='

printResult :: (Show a) => Either ServiceError a -> IO ()
printResult (Left err) = putStrLn $ " Помилка: " ++ show err
printResult (Right val) = putStrLn $ "Успіх: " ++ show val

main :: IO ()
main = do
    printHeader "СИСТЕМА УПРАВЛІННЯ ФАКУЛЬТЕТСЬКИМИ ПОСЛУГАМИ"

    -- Підключення до БД
    putStrLn "\n📊 Підключення до бази даних..."
    eConn <- connectDB
    case eConn of
        Left err -> do
            putStrLn $ " Не вдалося підключитися: " ++ show err
            return ()
        Right conn -> do
            putStrLn "✓ Підключення успішне"

            -- Ініціалізація БД
            printHeader "1. ІНІЦІАЛІЗАЦІЯ БАЗИ ДАНИХ"
            eInit <- initDatabase conn
            case eInit of
                Left err -> do
                    putStrLn $ " Помилка: " ++ show err
                    close conn
                    return ()
                Right _ -> putStrLn "✓ БД ініціалізована успішно"

            -- Наповнення даними
            printHeader "2. ЗАПОВНЕННЯ ДАНИМИ"
            ePop <- populateDatabase conn
            case ePop of
                Left err -> putStrLn $ " Помилка: " ++ show err
                Right _ -> putStrLn "✓ БД заповнена успішно"

            -- Показання всіх послуг
            printHeader "3. ВСІ ДОСТУПНІ ПОСЛУГИ"
            eServices <- findAll conn :: IO (Either ServiceError [Service])
            case eServices of
                Left err -> printResult (Left err)
                Right services -> do
                    putStrLn $ displayTable services
                    printHeader "3.1 ДЕТАЛІ ПОСЛУГ"
                    mapM_ (\s -> putStrLn (display s)) services

            -- Показання всіх авторів
            printHeader "4. ВСІ АВТОРИ ПОСЛУГ"
            eAuthors <- findAll conn :: IO (Either ServiceError [Author])
            case eAuthors of
                Left err -> printResult (Left err)
                Right authors -> do
                    putStrLn $ displayTable authors
                    printHeader "4.1 ДЕТАЛІ АВТОРІВ"
                    mapM_ (\a -> putStrLn (display a)) authors

            -- Показання всіх користувачів
            printHeader "5. ЗАРЕЄСТРОВАНІ КОРИСТУВАЧІ"
            eUsers <- findAll conn :: IO (Either ServiceError [User])
            case eUsers of
                Left err -> printResult (Left err)
                Right users -> do
                    putStrLn $ displayTable users
                    printHeader "5.1 ДЕТАЛІ КОРИСТУВАЧІВ"
                    mapM_ (\u -> putStrLn (display u)) users

            -- Активні користувачі
            printHeader "6. АКТИВНІ КОРИСТУВАЧІ"
            eActiveUsers <- getActiveUsers conn
            case eActiveUsers of
                Left err -> printResult (Left err)
                Right activeUsers -> do
                    putStrLn $ displayTable activeUsers
                    putStrLn $ printf "\nВсього активних користувачів: %d" (length activeUsers)

            -- Послуги за типом
            printHeader "7. ПОСЛУГИ ПО ТИПАМ"
            eEducational <- findServicesByType conn "Educational"
            case eEducational of
                Left err -> putStrLn $ "Educational: " ++ show err
                Right services -> do
                    putStrLn "📚 Educational послуги:"
                    if null services
                        then putStrLn "  Не знайдено"
                        else putStrLn $ displayTable services

            -- Топ послуги
            printHeader "8. ТОП 5 НАЙПОПУЛЯРНІШИХ ПОСЛУГ"
            eTopServices <- getTopServices conn 5
            case eTopServices of
                Left err -> printResult (Left err)
                Right topServices -> do
                    mapM_ (\(name, count) ->
                        printf "  📊 %s - %d використань\n" (T.unpack name) count) topServices

            -- Статистика за послугами
            printHeader "9. ДЕТАЛЬНА СТАТИСТИКА ПОСЛУГ"
            eAllServices <- findAll conn :: IO (Either ServiceError [Service])
            case eAllServices of
                Left err -> printResult (Left err)
                Right services -> do
                    forM_ services $ \service -> do
                        case serviceId service of
                            Nothing -> return ()
                            Just sid -> do
                                eReport <- getServiceReport conn sid
                                case eReport of
                                    Left err -> putStrLn $ "Помилка: " ++ show err
                                    Right (Just report) -> putStrLn (display report)
                                    Right Nothing -> return ()

            -- Активність користувачів
            printHeader "10. АКТИВНІСТЬ КОРИСТУВАЧІВ"
            eAllUsers <- findAll conn :: IO (Either ServiceError [User])
            case eAllUsers of
                Left err -> printResult (Left err)
                Right users -> do
                    forM_ users $ \user -> do
                        case userId user of
                            Nothing -> return ()
                            Just uid -> do
                                eActivity <- getUserActivity conn uid
                                case eActivity of
                                    Left err -> return ()
                                    Right activity -> do
                                        if null activity
                                            then return ()
                                            else do
                                                printf "\n👤 %s:\n" (T.unpack $ userName user)
                                                mapM_ (\(sname, count) ->
                                                    printf "   - %s: %d доступів\n"
                                                    (T.unpack sname) count) activity

            -- Статистика використання
            printHeader "11. СТАТИСТИКА ВИКОРИСТАННЯ"
            eStats <- findAll conn :: IO (Either ServiceError [UsageStatistics])
            case eStats of
                Left err -> printResult (Left err)
                Right stats -> do
                    printf "Всього записів про використання: %d\n" (length stats)
                    let totalDuration = sum $ map duration stats
                    printf "Загальна тривалість сеансів: %d хвилин\n" totalDuration

                    let actionCounts = length $ filter (\s -> actionType s == "login") stats
                    printf "Входів: %d\n" actionCounts

                    let sessionCounts = length $ filter (\s -> actionType s == "session") stats
                    printf "Сеансів: %d\n" sessionCounts

            -- Завершення
            printHeader "ЗАВЕРШЕННЯ"
            close conn
            putStrLn "✓ З'єднання закрито"
            putStrLn "✓ Програма завершена успішно\n"