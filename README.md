To use this:

1. run the url generating script by first tunneling to the prod db and then running
`./scripts/urls.sh
2. run `stack build`
3. then run 
  `$(stack path --local-install-root)/bin/prerender-cache-warmer FILENAME`
  where FILENAME is one of the url files generated in step 1 (TODO: make this better)
4. Do that with all the generated url files

Other notes:
1. Probably ok (better?) to run this on aws
2. Prerender has limits on the number of recaches we can do per min. To avoid this (esp. when we need to recahce the entire site), 
we first have to email them (support@prerender.io) and ask them to clear our cache. Then, change the first argument to `crawlConcurrently` from
`recache'` to `crawl`. This simulates a googlebot crawling the site, which causes the pages to be cached automatically (without limits), but only if the cache is empty. 
(TODO: make this better if we find that we're doing it often)
3. Keep an eye out on the site to make sure everything is still working properly when doing a "prewarm"
4. Vfiles and people have a worker setup to recache those pages (via addJob) as they change
