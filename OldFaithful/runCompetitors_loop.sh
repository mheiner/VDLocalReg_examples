for ii in {1..100..1}
do
  for puse in 1 2 3
  do
    for dte in 220816
	  do
      echo "nice -n 5 Rscript --no-restore 1_fit_competitors.R $puse $ii $dte &> progress/fitCompetitors\_p$puse\_ii$ii\_date$dte.txt"
    done
  done
done | parallel --jobs 4 # if using GNU Parallel
wait
