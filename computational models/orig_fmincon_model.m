%% GR and YC 4.13.21 modified for peer risk study from
%% GR 3.15.18 modified for DRT task from
%% GR 8.17.17 modified for MBMF from
%% HR 7.17.17 modified RWFit.m from
%https://github.com/krm58/Reinforcement-Learning-Models/tree/master/CreditAssignment
%to fit PIT data

function [lik] = orig_fmincon_model(subdata,alpha,beta) 

noRes = find(ismissing(subdata.response_self,'NA'));
subdata(noRes,:) = [];

trialNum = height(subdata); 
v1 = 5*ones(trialNum,1); 
p1 = ones(length(v1),1); 
v2 = subdata.Amount; 
p2 = subdata.Chance./100; 
choice_str =  subdata.response_self;
choice = NaN(length(choice_str),1);
for t=1:trialNum
    if strcmp(choice_str{t}, 'chance') == 1 
       choice(t) = 1;
    elseif  strcmp(choice_str{t}, 'certain') == 1
       choice(t) = 0;
    else
        error("no response detected for a trial, debug");
    end
end

u1 = p1.*(v1.^alpha);
u2 = p2.*(v2.^alpha);

p = 1 ./ (1 + exp(beta*(u1-u2))); 

% Trap log(0)
ind = p == 1;
p(ind) = 0.9999;
ind = p == 0;
p(ind) = 0.0001;
% Log-likelihood
err = (choice==1).*log(p) + (1 - (choice==1)).*log(1-p); 
lik = -sum(err);

end



