for modtype in Mean Reg
do
  for puse in 8
  do
    for ntn in 400
    do
	    for sigupper in 0.9
	    do
	      for sim_type in NNiChisq_indep
	      do
	        for s0 in 0.4
	        do
	          for ii in {1..20..1}
	          do
	            for dte in {23091301..23091303..1}
	            do
		            echo "Rscript --no-restore 1_fitPPMxR.R $modtype $puse $ntn $s0 $ii $dte &> progress/fitPPMxR_modtype$modtype\_p$puse\_n$ntn\_sigupper$sigupper\_simtype$sim_type\_s0$s0\_ii$ii\_date$dte.txt"
		          done
		        done
		      done
		    done
		  done
		done
	done
done | parallel --jobs 120
wait
