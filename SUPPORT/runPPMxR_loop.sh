for modtype in Mean Reg
do
  for puse in 6 10
  do
    for alph in 1.0
    do
	    for sigupper in 0.9
	    do
	      for sim_type in NNiChisq_indep
	      do
	        for s0 in 0.5
	        do
	          for ii in {1..20..1}
	          do
	            for dte in 220813
	            do
		            echo "Rscript --no-restore 1_fitPPMxR.R $modtype $puse $alph $sigupper $sim_type $s0 $ii $dte &> progress/fitPPMxR_modtype$modtype\_p$puse\_alph$alph\_sigupper$sigupper\_simtype$sim_type\_s0$s0\_ii$ii\_date$dte.txt"
		          done
		        done
		      done
		    done
		  done
		done
	done
done | parallel --jobs 112 # if using GNU Parallel
wait
