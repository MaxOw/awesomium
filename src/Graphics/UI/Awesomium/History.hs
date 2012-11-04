----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Awesomium.History
-- Copyright   :  (c) 2012 Maksymilian Owsianny
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- Maintainer  :  Maksymilian.Owsianny+Awesomium@gmail.com
-- Stability   :  Experimental
-- Portability :  Portable? (needs FFI)
--
----------------------------------------------------------------------

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

import Foreign.Ptr

-- | Destroy the instance (you must call this once you're done using
-- the instance)
queryResultDestroy :: HistoryQueryResult -> IO ()
queryResultDestroy = awe_history_query_result_destroy

-- | Get the total number of entries
queryResultGetSize :: HistoryQueryResult -> IO Int
queryResultGetSize = awe_history_query_result_get_size

-- | Get a certain entry (you must destroy any returned entry using
-- 'entryDestroy'). May return Nothing if the index is out of
-- bounds.
queryResultGetEntryAtIndex :: HistoryQueryResult -> Int
                           -> IO (Maybe HistoryEntry)
queryResultGetEntryAtIndex i r =
    awe_history_query_result_get_entry_at_index i r >>= \res ->
    if (res == nullPtr)
        then return Nothing
        else return $ Just res

-- | Destroy the instance
entryDestroy :: HistoryEntry -> IO ()
entryDestroy = awe_history_entry_destroy

-- | Get the URL of the page
entryGetUrl :: HistoryEntry -> IO String
entryGetUrl = awe_history_entry_get_url

-- | Get the title of the page
entryGetTitle :: HistoryEntry -> IO String
entryGetTitle = awe_history_entry_get_title

-- | Get the last time this page was visited (in seconds since epoch)
entryGetVisitTime :: HistoryEntry -> IO Double
entryGetVisitTime = awe_history_entry_get_visit_time

-- | Get the number of times this page was visited.
entryGetVisitCount :: HistoryEntry -> IO Int
entryGetVisitCount = awe_history_entry_get_visit_count

