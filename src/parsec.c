#include <stdio.h> /* per scrivere l'output eventualmente su un file*/
#include <stdlib.h> /* per la funzione rand */
// #include <time.h> /* per inizializzare la funzione rand */
#include <R.h> /* per comunicare con R */
#include <Rinternals.h>
// #include <math.h>

/* la matrice zeta di un ordinamento lineare e' una matrice ''triangolare'' */
void linzeta(int *lin, int *n, int *result) {
    int nval = *n;
    int pos;
    
    for(int i = 0; i < nval; i++) {
        for(int j = 0; j < nval; j++) {
            pos = nval*(lin[i]-1) + (lin[j]-1);
            if(i <= j) result[pos] = 1;
            else result[pos] = 0;
        }
    }
}

// funzione che genera nuove estensioni lineari... se necessario
// se lo fa restituisce 1 altrimenti 0
int new_linext(
    int n,
	int *linext,
	int *z
)
{
    int c = (int)floor(unif_rand()*RAND_MAX) % 2; // scelgo se generare una nuova estensione lineare
	if (c == 0)
	    return 0; // inutile fare altri conti e riciclare i precedenti
	
	int p = (int)floor(unif_rand()*RAND_MAX) % (n - 1); // posizione da scambiare con la successiva
	
	int pos = n * linext[p] + linext[p+1]; // determino la posizione in z
	if (z[pos] == 0) { // verifico se puo' avvenire lo scambio
	    // faccio lo scambio
		int tmp = linext[p];
		linext[p] = linext[p+1];
		linext[p+1] = tmp;
		return 1; // lo scambio e' avvenuto
	}
	
	// sebbene si abbia provato a generare una nuova estensione lineare,
	// lo scambio ha generato un ordinamento completo non compatibile col poset
	return 0;
}

// funzione principale, che esegue il loop e lancia le altre funzioni necessarie
// a generare le estensioni lineari e ad eseguire i conti su di esse
void bd(
    int    *linext,    // !!! estensione lineare come ranghi da 0 a n-1
	int    *n_scal,    // !!! numero di elementi, _scal indica che e' uno
	                   // scalare e permette di dare un nome piu' semplice e
				       // chiaro alla variabile che viene effettivamente usata
	int    *nit_scal,  // numero di iterazioni
	int    *z,         // !!! matrice di copertura traslata
    int    *rankfreq,  // matrice profili x ranghi che conta le rispettive
	                   // frequenze assolute
    int    *threshold, // soglia indicata come vettore di 0 e 1 se incluso o no
	int    *thrfreq,   // frequenza con la quale un elemento e' threshold
	                   // nell'estensione lineare
	int    *loweqthr,  // conta il numero di volte che un elemento e' sotto o
                       // uguale alla soglia
	double *weights,   // pesi da attribuire a ciascun profilo
	double *distances, // distanze tra i profili qualora siano confrontabili
	                   // che rappresentano i pesi degli edges che li collegano
					   // necessarie per valutare i gap pesati
    double *cumdist,   // vettore delle distanze cumulate dal basso
	                   // nell'estensione lineare, inizializzate a 0,
					   // strumentale al calcolo del gap
    double *gapAP,     // gap assoluto di poverta'
	double *gapRP,     // gap relativo di poverta'
	double *gapAR,     // gap assoluto di ricchezza
	double *gapRR,     // gap relativo di ricchezza
	double *polarization_scal // indice di polarizzazione
)
{
    // inizializzazione della random seed
    // srand(seed);
	GetRNGstate();
    
	// importo gli scalari
    int n = *n_scal;
	int nit = *nit_scal;
	
	// variabili di supporto
	int new = 0;
	int pos;
	int linthr = 0; // threshold nell'estensione lineare
	double tmppolar = 0;
	
	for (int w = 0; w < nit; w++) { // LOOP principale
	    
		new = new_linext(n, linext, z);
		
		// qua ci sono tutte le operazioni necessarie ogni qual volta che si
		// cambia l'estensione lineare
		if (w == 0 || new == 1) {
		
		    linthr = 0;
			for (int i = 0; i < n; i++) {
			    // calcolo le "distanze cumulate"
				if (i > 0) {
				    pos = n * linext[i-1] + linext[i];
				    cumdist[linext[i]] = cumdist[linext[i-1]] + distances[pos];
				}
				else
				    cumdist[linext[0]] = 0;
			    // trovo la soglia dell'estensione lineare
			     if (threshold[linext[i]]) {
				     linthr = i;
				}
			}
						
			// calcolo l'indice di polarizzazione
			tmppolar = 0;
			if (polarization_scal[0] >= 0)
			    for (int i = 0; i < n; i++) for (int j = i + 1; j < n; j++) {
			        tmppolar += weights[linext[j]]*weights[linext[i]]*(j - i);
			    }
		}
		
		// aggiorno la media della polarizzazione, media e non somma per non
		// avere numeri troppo elevati
		if (polarization_scal[0] >= 0)
		    polarization_scal[0] = (polarization_scal[0] * w + tmppolar)/(w+1);
		
		// frequenza assoluta di soglia
		thrfreq[linext[linthr]]++;
		
		// conti che vanno a fatti a prescindere dal cambio di estensione
		// lineare
	    for (int i = 0; i < n; i++) {
		    // conto le frequenze dei ranghi
		    pos = n * linext[i] + i;
			rankfreq[pos]++;
			if (i <= linthr) {
			    // frequenze assolute delle volte che un elemento e' minore o
				// uguale alla soglia
				loweqthr[linext[i]]++;
				// calcolo il gap assoluto di poverta'
			    // il +1 serve a spostarsi al primo profilo non di poverta'
				gapAP[linext[i]] +=
			        cumdist[linext[linthr +1]] - cumdist[linext[i]];
			    // calcolo il gap relativo di poverta' dividendo per il massimo
				// valore possibile
			    gapRP[linext[i]] +=
			        1 - cumdist[linext[i]]/cumdist[linext[linthr +1]];
			}
			if (i > linthr) {
			    // calcolo il gap assoluto di ricchezza
			    // il +1 non serve perche' il primo profilo di poverta' e' la
				// soglia stessa
				gapAR[linext[i]] +=
			        cumdist[linext[i]] - cumdist[linext[linthr]];
			    // calcolo il gap relativo di ricchezza dividendo per il massimo
				// valore possibile
			    gapRR[linext[i]] +=
			        (cumdist[linext[i]] - cumdist[linext[linthr]])
					/(cumdist[linext[n-1]] - cumdist[linext[linthr]]);
			}
		}
	}
	PutRNGstate();
}

// funzione principale, che esegue il loop e lancia le altre funzioni necessarie
// a generare le estensioni lineari e ad eseguire i conti su di esse
// (solo l'identification function)
void bd_simp(
    int    *linext,    // !!! estensione lineare come ranghi da 0 a n-1
	int    *n_scal,    // !!! numero di elementi, _scal indica che e' uno
	                   // scalare e permette di dare un nome piu' semplice e
				       // chiaro alla variabile che viene effettivamente usata
	int    *nit_scal,  // numero di iterazioni
	int    *z,         // !!! matrice di copertura traslata
    int    *rankfreq,  // matrice profili x ranghi che conta le rispettive
	                   // frequenze assolute
    int    *threshold, // soglia indicata come vettore di 0 e 1 se incluso o no
	int    *thrfreq,   // frequenza con la quale un elemento e' threshold
	                   // nell'estensione lineare
	int    *loweqthr,  // conta il numero di volte che un elemento e' sotto o
                       // uguale alla soglia
	double *weights//, // pesi da attribuire a ciascun profilo
//	double *distances, // distanze tra i profili qualora siano confrontabili
//	                   // che rappresentano i pesi degli edges che li collegano
//					   // necessarie per valutare i gap pesati
//  double *cumdist,   // vettore delle distanze cumulate dal basso
//	                   // nell'estensione lineare, inizializzate a 0,
//					   // strumentale al calcolo del gap
//  double *gapAP,     // gap assoluto di poverta'
//	double *gapRP,     // gap relativo di poverta'
//	double *gapAR,     // gap assoluto di ricchezza
//	double *gapRR,     // gap relativo di ricchezza
//	double *polarization_scal // indice di polarizzazione
)
{
    // inizializzazione della random seed
    // srand(time(NULL));
	// srand(seed);
	GetRNGstate();
    
	// importo gli scalari
    int n = *n_scal;
	int nit = *nit_scal;
	
	// variabili di supporto
	int new = 0;
	int pos;
	int linthr = 0; // threshold nell'estensione lineare
//	double tmppolar = 0;
	
	for (int w = 0; w < nit; w++) { // LOOP principale
	    
		new = new_linext(n, linext, z);
		
		// qua ci sono tutte le operazioni necessarie ogni qual volta che si
		// cambia l'estensione lineare
		if (w == 0 || new == 1) {
		
		    linthr = 0;
			for (int i = 0; i < n; i++) {
			    // calcolo le "distanze cumulate"
//				if (i > 0) {
//				    pos = n * linext[i-1] + linext[i];
//				    cumdist[linext[i]] = cumdist[linext[i-1]] + distances[pos];
//				}
//				else
//				    cumdist[linext[0]] = 0;
			    // trovo la soglia dell'estensione lineare
			     if (threshold[linext[i]]) {
				     linthr = i;
				}
			}
						
			// calcolo l'indice di polarizzazione
//			tmppolar = 0;
//			if (polarization_scal[0] >= 0)
//			    for (int i = 0; i < n; i++) for (int j = i + 1; j < n; j++) {
//			        tmppolar += weights[linext[j]]*weights[linext[i]]*(j - i);
//			    }
		}
		
		// aggiorno la media della polarizzazione, media e non somma per non
		// avere numeri troppo elevati
//		if (polarization_scal[0] >= 0)
//		    polarization_scal[0] = (polarization_scal[0] * w + tmppolar)/(w+1);
		
		// frequenza assoluta di soglia
		thrfreq[linext[linthr]]++;
		
		// conti che vanno a fatti a prescindere dal cambio di estensione
		// lineare
	    for (int i = 0; i < n; i++) {
		    // conto le frequenze dei ranghi
		    pos = n * linext[i] + i;
			rankfreq[pos]++;
			if (i <= linthr) {
			    // frequenze assolute delle volte che un elemento e' minore o
				// uguale alla soglia
				loweqthr[linext[i]]++;
				// calcolo il gap assoluto di poverta'
			    // il +1 serve a spostarsi al primo profilo non di poverta'
//				gapAP[linext[i]] +=
//			        cumdist[linext[linthr +1]] - cumdist[linext[i]];
			    // calcolo il gap relativo di poverta' dividendo per il massimo
				// valore possibile
//			    gapRP[linext[i]] +=
//			        1 - cumdist[linext[i]]/cumdist[linext[linthr +1]];
			}
//			if (i > linthr) {
			    // calcolo il gap assoluto di ricchezza
			    // il +1 non serve perche' il primo profilo di poverta' e' la
				// soglia stessa
//				gapAR[linext[i]] +=
//			        cumdist[linext[i]] - cumdist[linext[linthr]];
			    // calcolo il gap relativo di ricchezza dividendo per il massimo
				// valore possibile
//			    gapRR[linext[i]] +=
//			        (cumdist[linext[i]] - cumdist[linext[linthr]])
//					/(cumdist[linext[n-1]] - cumdist[linext[linthr]]);
//			}
		}
	}
	PutRNGstate();
}
