for puse in 8
do
  for ntn in 400
  do
    for ii in {1..50..1}
    do
      for dte in 230913
	    do
        echo "Rscript --no-restore 1_fit_competitors.R $puse $ntn $ii $dte &> progress/fitCompetitors\_p$puse\_n$ntn\_ii$ii\_date$dte.txt"
      done
    done
  done
done | parallel --jobs 3
wait
