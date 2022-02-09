% on réutilise les données calculé dans la demodulation par filtrage
Demodulation_filtrage;

%% 4.1 Demodulateur FSK - Contexte de synchronisation idéale

% On considère que les déphasages phi1 et phi0 sont connus
% On reproduit alors les équations de la figure 5 du sujet
x0 = (x_noise.*cos(2*pi*F0*t + phi0)); 

x1 = (x_noise.*cos(2*pi*F1*t + phi1));
X0 = zeros(1,n);
X1 = zeros(1,n);
for i = 1:n
    X0(i) = dt * sum( x0((i-1)*Ns+1:i*Ns) );
    X1(i) = dt * sum( x1((i-1)*Ns+1 : i*Ns) );
end 
Bits = X1-X0;
Bits = Bits - mean(Bits);

signal_result = Bits > 0;
Taux_erreur_4_1 = sum(signalBin ~= signal_result)/n

% Afficher les résultats
NRZ_sortie = construire_NRZ(n, Ns, signal_result);
afficher_resultats(t, x_noise, x_pb, x_ph, NRZ, NRZ_sortie, '4.1: Demodulateur FSK - Contexte de synchronisation idéale')


%% 4.2 Démodulateur FSK avec gestion d'une erreur de synchronisation de phase porteuse

% Sur ce cas, on considère que les déphasages phi1 et phi0 sont inconnus
% On reproduits les équtions de la figure 5 du sujet: 
cosx0 = (x.*cos(2*pi*F0*t)); 
sinx0 = (x.*sin(2*pi*F0*t));
cosx1 = (x.*cos(2*pi*F1*t));
sinx1 = (x.*sin(2*pi*F1*t)); 

cosX0 = zeros(1, n);
cosX1 = zeros(1, n);
sinX0 = zeros(1, n);
sinX1 = zeros(1, n);
for i = 1:n
    cosX0(i) = dt * sum( cosx0((i-1)*Ns+1:i*Ns) );
    cosX1(i) = dt * sum( cosx1((i-1)*Ns+1 : i*Ns) );
    sinX0(i) = dt * sum( sinx0((i-1)*Ns+1:i*Ns) );
    sinX1(i) = dt * sum( sinx1((i-1)*Ns+1 : i*Ns) );
end 
XO = cosX0.^2 + sinX0.^2;
X1 = cosX1.^2 + sinX1.^2;
Bits = (X1-X0)*1000;
Bits = Bits - mean(Bits);

signal_result = Bits > 0;
Taux_erreur_4_2 = sum(signalBin ~= signal_result)/n

% Afficher les résultats
NRZ_sortie = construire_NRZ(n, Ns, signal_result);
afficher_resultats(t, x_noise, x_pb, x_ph, NRZ, NRZ_sortie, '4.2: Démodulateur FSK avec gestion d''une erreur de synchronisation de phase porteuse')
