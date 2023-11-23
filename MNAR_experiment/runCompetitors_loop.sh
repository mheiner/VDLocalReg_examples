for ii in {1..100..1}
do
  for missType in MCAR MAR MNAR
  do
	    for dataType in Additive Product
	    do
        for puse in 2
        do
          for dte in 230829
	        do
      echo "Rscript --no-restore 1_fit_competitors.R $missType $dataType $puse $ii $dte &> progress/fitCompetitors\_mist$missType\_dtype$dataType\_p$puse\_ii$ii\_date$dte.txt"
          done
        done
      done
  done
done | parallel --jobs 3
wait
