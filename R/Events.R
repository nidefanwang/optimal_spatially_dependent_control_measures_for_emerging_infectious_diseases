
############################ Events corresponding to Rate matrix ############################

# compartments
names.compartment  <-  c('S', 'E.m', 'E.s', 'I.m', 'I.s', 'CE.m', 'CE.s', 'CI.m', 'CI.s', 'H', 'R');
temp               =   names.compartment;
names.state        <-  matrix(0,length(temp),n);
for(i in 1:length(temp)){names.state[i,] = paste0(temp[i],1:n)}
names.state        =   c(t(names.state));

# events
temp1  <-  c();
temp2  <-  c('M.S', 'M.E.m', 'M.E.s', 'M.I.m', 'M.I.s', 'M.R', 
             'Infec.m', 'Infec.s', 
             'Lat.E.m', 'Lat.E.s', 'Lat.CE.m', 'Lat.CE.s',
             'Tra.E.m', 'Tra.E.s', 'Tra.I.m', 'Tra.I.s', 
             'Del.m', 'Del.s', 
             'Hos.I.s', 'Hos.CI.s',
             'Rec.I.m', 'Rec.CI.m', 'Rec.H');             
temp3  <-  c(rep(n^2,6), rep(n,2), rep(n,4), rep(n,4), rep(n,2), rep(n,2), rep(n,3));
for(i in 1:length(temp3)){temp1  =   c(temp1,rep(temp2[i],temp3[i]))}

# event matrix, columns are compartments, rows are events
Events  <-  matrix(0, length(temp1), length(names.state));
colnames(Events)  =   names.state;
rownames(Events)  =   temp1;

# 1. population movement events (M.S, M.E.m, M.E.s, M.I.m, M.I.s, M.R) (6n^2 rows)
temp  =   c('S', 'E.m', 'E.s', 'I.m', 'I.s', 'R');
for(k in 1:6){
  for(i in 1:n){
    for(j in 1:n){
      rownumber  = min(which(rownames(Events)==(unique(rownames(Events))[k]))) + (i-1)*n + (j-1);
      colnumber1 = which(colnames(Events)==paste0(temp[k],i));
      colnumber2 = which(colnames(Events)==paste0(temp[k],j));
      if(j!=i){
        Events[rownumber,colnumber1] = -1;
        Events[rownumber,colnumber2] = +1;         
      }
    }
  }
}

# transition function for transition events which contain only n subevents and dont cross regions (Events is the matrix to be operated on; event is the transition event used to locate the rows of the Events matrix and needs to be the element of rownames(Events), this event is transferred from state1 to state2)
transition <- function(event, state1, state2, Events){
  for(i in 1:n){
    rownumber  = min(which(rownames(Events)==event)) + (i-1);
    colnumber1 = which(colnames(Events)==paste0(state1, i));
    colnumber2 = which(colnames(Events)==paste0(state2, i));
    Events[rownumber,colnumber1] = -1; 
    Events[rownumber,colnumber2] = +1;
  }
  return(Events);
}

# 2. new infection events (2n rows) 
Events = transition(event = 'Infec.m',  state1 = 'S',    state2 = 'E.m',  Events = Events);
Events = transition(event = 'Infec.s',  state1 = 'S',    state2 = 'E.s',  Events = Events);

# 3. events for transition from latent period to infectious period (4n rows)
Events = transition(event = 'Lat.E.m',  state1 = 'E.m',  state2 = 'I.m',  Events = Events);
Events = transition(event = 'Lat.E.s',  state1 = 'E.s',  state2 = 'I.s',  Events = Events);
Events = transition(event = 'Lat.CE.m', state1 = 'CE.m', state2 = 'CI.m', Events = Events);
Events = transition(event = 'Lat.CE.s', state1 = 'CE.s', state2 = 'CI.s', Events = Events);

# 4. isolation events of contact tracing
Events = transition(event = 'Tra.E.m',  state1 = 'E.m',  state2 = 'CE.m', Events = Events);
Events = transition(event = 'Tra.E.s',  state1 = 'E.s',  state2 = 'CE.s', Events = Events);
Events = transition(event = 'Tra.I.m',  state1 = 'I.m',  state2 = 'CI.m', Events = Events);
Events = transition(event = 'Tra.I.s',  state1 = 'I.s',  state2 = 'CI.s', Events = Events);

# 5. isolation events of other way
Events = transition(event = 'Del.m',    state1 = 'I.m',  state2 = 'CI.m', Events = Events);
Events = transition(event = 'Del.s',    state1 = 'I.s',  state2 = 'CI.s', Events = Events);

# 6. hospitalization of severe cases
Events = transition(event = 'Hos.I.s',  state1 = 'I.s',  state2 = 'H',    Events = Events);
Events = transition(event = 'Hos.CI.s', state1 = 'CI.s', state2 = 'H',    Events = Events);

# 7. recovery
Events = transition(event = 'Rec.I.m',  state1 = 'I.m',  state2 = 'R',    Events = Events);
Events = transition(event = 'Rec.CI.m', state1 = 'CI.m', state2 = 'R',    Events = Events);
Events = transition(event = 'Rec.H',    state1 = 'H',    state2 = 'R',    Events = Events);
############################ Events corresponding to Rate matrix ############################
