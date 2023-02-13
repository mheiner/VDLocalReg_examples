for ii in {1..100..1}
do
  for missType in MAR MNAR
  do
	  for perMiss in 0 0.1 0.25 0.5
	  do
	    for dataType in 1 2
	    do
        for puse in 10
        do
          for dte in 230123
	        do
      echo "Rscript --no-restore 1_fit_competitors.R $missType $perMiss $dataType $puse $ii $dte &> progress/fitCompetitors\_mist$missType\_pmis$perMiss\_dtype$dataType\_p$puse\_ii$ii\_date$dte.txt"
          done
        done
      done
    done
  done
done | parallel --jobs 3 # if using GNU Parallel
wait
