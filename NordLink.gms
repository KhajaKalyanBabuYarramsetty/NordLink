
*#####################     Declaring Sets   #################################
Sets
         i       set of conventional generation technologies
         r       set of RES generation technologies     /Wind, PV/
         t       set of hours                   /1*8760/
         n       set of nodes (countries)       /NO, GER/
;
alias (n,nn)
* Alias(Set_Name, Alternate_Name) defines a synonym for a set
;

*#####################     Declaring Parameters   #############################
Parameters
         vc(i)           unit cost (? per MWh)
         sc(i)           startup costs (? per MW)
         cap(i,n)        installed capacity for each technology i
         cap_RES(r,n)    installed capacity for each RES technology r
         pf(t,r,n)       hourly production factor for RES
         af(i)           availability factor for conv. technologies
         demand(t,n)     electricity demand
         g_min(i)        minimum generation
         ntc(n,nn)       transmission capacities from node n to node nn
         techup          technology upload
         
**************** Implement HP parameters ************************************
  efcy      efficiency of hydro power plant
  avhr        avaliability factor of the hydro reservoir
  hpc(n)     installed capacity in hydro power plant
  hrc(n)     capacity of hydro reservoir

;

*#######################  Upload from Excel Excel ##############################

*1.Create Text File which includes the information where which parameter is to find
$onecho > ImportInfo.txt
set=i       Rng=Technology!A2           Cdim=0 Rdim=1
Par=techup  Rng=Technology!A1:F10       Cdim=1 Rdim=1
Par=demand  Rng=Demand!A2               Cdim=1 Rdim=1
Par=pf      Rng=RES!A1                  Cdim=2 Rdim=1
Par=cap     Rng=Technology!G2           Cdim=1 Rdim=1
Par=ntc     Rng=NTC!A2                  Cdim=1 Rdim=1
$offecho

*2.Convert Excel File to a .gdx file
$call GDXXRW I=Input_NL.xlsx O=Output.gdx @ImportInfo.txt

*3.Read the elements
$gdxin Output.gdx
$Load i
$Load techup, demand, cap, pf, ntc
$gdxin

*Define parameters from the upload-parameter 'techup'
        vc(i)=techup(i,'vc');
        sc(i)=techup(i,'sc');
        g_min(i)=techup(i,'g_min');
        af(i)=techup(i,'af');

*Define renewable parameters from the input file
        cap_RES('wind','NO')  = 1188;
        cap_RES('wind','GER') = 56130;
        cap_RES('pv','NO')    = 400;
        cap_RES('pv','GER')   = 54300;
        
**************** Declare HP parameters ************************************
    avhr = 0.85;
    efcy = 0.92;
    
    hpc('NO') = 23475;
    hpc('GER') = 0;
    hrc('NO') = 135000000;
    hrc('GER') = 0;



*Display data to see that everything is uploaded correctly
Display i, t, vc, sc, g_min, cap,cap_RES, af, demand, ntc, pf;
*$stop


*#####################     Declaring Variables   #############################
Variable
         COST              total cost of electricity production
;

Positive Variables
         G(i,n,t)          generation of technology i at time t
         G_RES(r,n,t)      generation of RES technology r at time t
         P_ON(i,n,t)       online capacity of technology i at time t
         SU(i,n,t)         start up variable
         FLOW(n,nn,t)      electrictity flow from node n to node nn in time t

**************** Implement HP variables ************************************
  GHP(n,t)    hydropower generation

;

*#####################     Declaring Equations   #############################
Equations
        obj                minimizing total costs
        res_dem            energy balance (supplly=demand)
        res_start          startup restriction
        res_G_RES          maximum for RES generation depends on hourly pf anc cap_RES
        res_min_gen        minimum generation
        res_max_gen        maximum generation
        res_max_online     maximum online restriction
        res_flow_1         export has to be smaller than the transmission limit
        res_flow_2         import has to be smaller than the transmission limit
        
**************** Implement HP equations ************************************
  CRHP    Hydropower constraint capacity
  CRHPR   Hydroreservoir constraint capacity

;

obj..                      COST    =E= SUM((i,n,t), vc(i)* G(i,n,t)+sc(i)*SU(i,n,t))
;
res_dem(n,t)..             SUM(i,  G(i,n,t))+SUM(r, G_RES(r,n,t)) + GHP(n,t) =E= demand(t,n) + sum(nn,FLOW(n,nn,t) - FLOW(nn,n,t))
;
res_G_RES(r,n,t)..         G_RES(r,n,t) =L= cap_RES(r,n)* pf(t,r,n)
;
res_start(i,n,t)..         SU(i,n,t) =G= P_ON(i,n,t)-P_ON(i,n,t-1)
;
res_min_gen(i,n,t)..       P_ON(i,n,t)*g_min(i) =L= G(i,n,t)
;
res_max_gen(i,n,t)..       G(i,n,t)    =L= P_ON(i,n,t)
;
res_max_online(i,n,t)..    P_ON(i,n,t) =L= cap(i,n)*af(i)
;
res_flow_1(n,nn,t)..       FLOW(n,nn,t) =L= ntc(n,nn)
;
res_flow_2(n,nn,t)..       FLOW(nn,n,t) =L= ntc(nn,n)
;
**************** Declare HP equations ************************************
CRHP(n,t)..          GHP(n,t) =L= (hpc(n)*efcy) * avhr
;
CRHPR(n)..          SUM((t), GHP(n,t)) =L= hrc(n)
;



* #########################    Solving the Model  ##############################
model PSE
/  all/
;

solve PSE using LP minimizing COST
;

* #########################    Reporting  ######################################
*Declaring Report Parameters
        parameter
        generation(t,*,n,i)
        price(t,*,n)
        flow_ger_no(t,*,n,nn)
        startup(*,i,n,t)
        modelstats(*)
        solvestats(*)
;

        generation(t,'bau',n,i) = G.l(i,n,t);
        price(t,'bau',n)        = res_dem.M(n,t);
        flow_ger_no(t,'flows',n,nn) = FLOW.l(n,nn,t);
* Marginal  of Energy Balance gives the price (Shadow variable of the energy balance)

        startup('bau',i,n,t)    = SU.L(i,n,t);
        modelstats('bau')       = PSE.modelstat;
        solvestats('bau')       = PSE.modelstat;

display generation, price, flow_ger_no, startup, modelstats, solvestats  ;

*  or: sometimes it is enough to just display the results in GAMS.
*  e.g.:
display G.L, P_ON.L, res_dem.M ;

*#################################  Excel Export  ###############################
*1. Create Text file which includes the information where which parameter is to put
$onecho >out.tmp
Par=generation           Rng=generation!A2   Cdim=3 Rdim=1
Par=price                Rng=Price!A2        Cdim=2 Rdim=1
Par=flow_ger_no          Rng=Flow!A2         Cdim=2 Rdim=2
Par=startup              Rng=StartUp!B2      Cdim=2 Rdim=2
Par=modelstats           Rng=Stats!B2        Cdim=0 Rdim=1
Par=solvestats           Rng=Stats!E2        Cdim=0 Rdim=1
$offecho

*2. Put the data in a .gdx file
execute_unload 'Results.gdx'
;
*3.Convert the .gdx file to an excel file
execute 'gdxxrw Results.gdx o=Results.xlsx @out.tmp'  ;
