%% HF and YC 6.2.23 modified for peer riskfrom
%% GR 4.13.21 modified for peer risk from
%% GR 3.15.18 modified for DRT task from
%% GR 8.17.17 modified for MBMF from
%% HR 7.17.17 modified RWFit.m from
%https://github.com/krm58/Reinforcement-Learning-Models/tree/master/CreditAssignment
%to fit PIT data

function [lik] = revMod(subdata,alpha,beta,weight_frd)

noRes = find(ismissing(subdata.response_self,'NA'));
subdata(noRes,:) = [];


trialNum = height(subdata); 
v1 = 5*ones(trialNum,1); 
p1 = ones(length(v1),1); 
v2 = subdata.Amount; 
p2 = subdata.Chance./100; 
frd_alpha = subdata.Alpha(1); % Alpha from FP 
choice_str =  subdata.response_self;
choice = NaN(length(choice_str),1);

for t=1:trialNum
    if strcmp(choice_str{t}, 'chance') == 1 %if simulation, change 'chance' to 'risky'
       choice(t) = 1;
    elseif  strcmp(choice_str{t}, 'certain') == 1
       choice(t) = 0;
    else
        error("no response detected for a trial, debug");
    end
end


u1 = (1-weight_frd) * p1 .* (v1.^alpha)+ weight_frd * p2.*(v2.^frd_alpha);
u2 = (1-weight_frd) * p2 .*(v2.^alpha) + weight_frd * p1.*(v1.^frd_alpha);

p = 1 ./ (1 + exp(beta*(u1-u2))); %probability of choosing option 2 (risky option)

% Trap log(0)
ind = p == 1;% find index of all p == 1, change them to .9999 so can compute log.
p(ind) = 0.9999;
ind = p == 0;
p(ind) = 0.0001;
% Log-likelihood
err = (choice==1).*log(p) + (1 - (choice==1)).*log(1-p); 
% Sum of -log-likelihood
lik = -sum(err);

end



