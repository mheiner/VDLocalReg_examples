for puse in 6 10
do
  for ii in {1..20..1}
  do
    for dte in 220813
	  do
      echo "Rscript --no-restore 1_fit_competitors.R $puse $ii $dte &> progress/fitCompetitors\_p$puse\_ii$ii\_date$dte.txt"
    done
  done
done | parallel --jobs 3 # if using GNU Parallel
wait
