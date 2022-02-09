clear;

F0 = 6000; %bit0
F1 = 2000; %bit1
Fe = 48000; %Frequence d'echantillonnage
Te = 1/Fe;


%3.1) 300 bits/s => Ns = Fe/300 
Ns = Fe/300;
Ts = Ns*Te;

n = 128;
signalBin = randi([0,1], [1,n]);

% Reconsitution de l'image: decommenter un des fichier aini que n et
% signalBin
%load 'Fichiers utiles au projet-20201208'/DonneesBinome1.mat
%load 'Fichiers utiles au projet-20201208'/DonneesBinome2.mat
%load 'Fichiers utiles au projet-20201208'/DonneesBinome3.mat
%load 'Fichiers utiles au projet-20201208'/DonneesBinome4.mat
%load 'Fichiers utiles au projet-20201208'/DonneesBinome5.mat
%load 'Fichiers utiles au projet-20201208'/DonneesBinome6.mat
%n = length(bits);
%signalBin = bits;


%3.2) Signal bruité
phi0 = rand*2*pi;
phi1 = rand*2*pi;
k = [1:n*Ns];
t = Te*[0:n*Ns-1];

SNRdB = 10;


% 3.3) Filtrage
ordre = 601;
N = (- (ordre-1)/2 : (ordre-1)/2);
fc = 3000;

K = 30; % seuil determiné experimentalement avec la fonction estimer_seuil_experimental


% 3.4) Recommandation V21 (A commenter pour executer les parties antérieurs
% ou décommenter pour les parties suivantes)
% F0 = 1180; %bit0
% F1 = 980; %bit1
% fc = 1080;


% 4)
dt = Te;