userEmailDomain :: User -> Maybe String
userEmailDomain user = case getContactInfo user of
    Nothing          -> Nothing
    Just contactInfo -> case getEmailAddress contactInfo of
        Nothing    -> Nothing
        Just email -> getDomain email

chainMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  `chainMaybe` f = Nothing
(Just x) `chainMaybe` f = fx

userEmailDomain :: User -> Maybe String
userEmailDomain user =
(Just user) `chainMaybe` getContactInfo
            `chainMaybe` getEmailAddress
            `chainMaybe` getDomain
