module Graphics.UI.Awesomium.History
    ( HistoryQueryResult, HistoryEntry
    , queryResultDestroy
    , queryResultGetSize
    , queryResultGetEntryAtIndex
    , entryDestroy
    , entryGetUrl
    , entryGetTitle
    , entryGetVisitTime
    , entryGetVisitCount
) where

import Graphics.UI.Awesomium.Raw

queryResultDestroy :: HistoryQueryResult -> IO ()
queryResultDestroy = awe_history_query_result_destroy

queryResultGetSize :: HistoryQueryResult -> IO Int
queryResultGetSize = awe_history_query_result_get_size

queryResultGetEntryAtIndex :: HistoryQueryResult -> Int -> IO HistoryEntry
queryResultGetEntryAtIndex = awe_history_query_result_get_entry_at_index

entryDestroy :: HistoryEntry -> IO ()
entryDestroy = awe_history_entry_destroy

entryGetUrl :: HistoryEntry -> IO String
entryGetUrl = awe_history_entry_get_url

entryGetTitle :: HistoryEntry -> IO String
entryGetTitle = awe_history_entry_get_title

entryGetVisitTime :: HistoryEntry -> IO Double
entryGetVisitTime = awe_history_entry_get_visit_time

entryGetVisitCount :: HistoryEntry -> IO Int
entryGetVisitCount = awe_history_entry_get_visit_count

