function [Kmax, coefmax, signalResult] = estimer_seuil_experimental(n, Ns, x_pb, signalBin)
coefmax = 1;
Kmax = 0;
for K = 0:0.01:Ns % La somme des carré du signal original ne peut jamais dépasser Ns, le bruit étant faible et de moyenne nulle cette borne de test est acceptable
    signal_result = zeros(n,1);
    for i = 0:n-1
        signal_result(i+1) = sum(x_pb(Ns*i+1:Ns*(i+1)).^2)>K; 
    end
    coef = sum(signalBin ~= signal_result)/n;
    if coef < coefmax
        coefmax = coef;
        Kmax = K;
    end
end