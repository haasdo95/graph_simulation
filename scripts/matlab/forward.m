function [S,E,I,R,t] = forward(beta,gamma,delta,S0,E0,I0,tspan)
%
% Input
%
%   beta: scalar:  infection rate
%   gamma: scalar:  Inverse of the average latent time
%   delta: scalar:  inverse of the average recovery time
%   S0: scalar: Initial proportion of susceptible group
%   E0: scalar: Initial proportion of exposed cases
%   I0: scalar: Initial proportion of infectious cases
%   tspan: [t0 tf] indicating start and end time
%
% Output
%   S: vector [Nx1] of the target time-histories of the susceptible cases
%   E: vector [Nx1] of the target time-histories of the exposed cases
%   I: vector [Nx1] of the target time-histories of the infectious cases
%   R: vector [Nx1] of the target time-histories of the recovered cases
%   t: adaptive time steps chosen by ODE45
%

%% Solve via ODE45
    function dQdt = SEIR(~, Q)
%       Q: [1x3] state vector of S, E, I
        S_ = -beta*Q(1)*Q(3);
        E_ = -S_-gamma*Q(2);
        I_ = gamma*Q(2)-delta*Q(3);
        dQdt = [S_; E_; I_];
    end
[t, Q] = ode45(@SEIR, tspan, [S0,E0,I0]);
num_steps = numel(t);
S = Q(:,1);
E = Q(:,2);
I = Q(:,3);
R = ones(num_steps, 1) - S - E - I;

end


