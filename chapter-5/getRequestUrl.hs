main = do
    print(getRequestURL "http://example.com" "1337hAsk3ll" "book" "1234")
    print(exampleUrlBuilder "1337hAsk3ll" "book" "1234")
    print(myExampleUrlBuilder "book" "1234")
    print(myResourceBuilder "1234")
    print(apiBuilderPartial "book" "1234")
    print(bookRequestBuilder "1234")

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

genHostRequestBuilder host = (\apiKey resource id ->
                                getRequestURL host apiKey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id ->
                                                hostBuilder apiKey resource id)

genResourceBuilder apiRequestBuilder resource = (\id -> apiRequestBuilder resource id )

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

myResourceBuilder = genResourceBuilder myExampleUrlBuilder "book"

-- Partial closures
exampleUrlBuilderPartial = getRequestURL "http://example.com"
apiBuilderPartial = exampleUrlBuilderPartial "1337hAsk3ll"
bookRequestBuilder = apiBuilderPartial "book"
