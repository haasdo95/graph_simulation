function [beta,gamma,delta] = backward(S,E,I,t,guess,lb,ub)
%% function f(param, x) for curve fitting
S0 = S(1);
E0 = E(1);
I0 = I(1);
tspan = [t(1) t(end)];
    function y = fitting_func(param, time_step)
        beta_ = param(1);
        gamma_ = param(2);
        delta_ = param(3);
        [S_,E_,I_,~,t_] = SEIR_forward(beta_,gamma_,delta_,S0,E0,I0,tspan);
        % interpolation must be done to compare two time sequences
        y = interp1(t_,[S_ E_ I_],time_step); % y of shape (time_step, 3)
    end
[param,~,~,~] = lsqcurvefit(@fitting_func,guess,t,[S E I],lb,ub);
beta = param(1);
gamma = param(2);
delta = param(3);
end

