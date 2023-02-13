for pmiss in 0 10 25 50
do
  for ii in {1..100..1}
  do
    for dte in 220813
	  do
      echo "Rscript --no-restore 1_fit_competitors.R $pmiss $ii $dte &> progress/fitCompetitors\_pMiss$pmiss\_ii$ii\_date$dte.txt"
    done
  done
done | parallel --jobs 4 # if using GNU Parallel
wait
