function afficher_resultats(t, x_bruitee, x_pb, x_ph, NRZ_entree, NRZ_sortie, nom)
figure('Name', nom, 'NumberTitle', 'off');
subplot(411);
plot(t, NRZ_entree);
title('Signal binaire en entree');
xlabel('Temps (s)');
ylabel('Amplitude');
ylim([-0.2, 1.2]);

subplot(412);
plot(t, x_bruitee)
title('Signal bruité modulé en fréquence');
xlabel('Temps (s)');
ylabel('Amplitude');
ylim([-1.2, 1.2]);

subplot(413);
plot(t,x_pb,'blue', t, x_ph, 'red');
title('Signal filtré');
legend('filtrage passe bas', 'filtrage passe haut');
xlabel('Temps (s)');
ylabel('Amplitude');

subplot(414);
plot(t, NRZ_sortie)
title('Signal binaire en sortie après démodulation');
xlabel('Temps (s)');
ylabel('Amplitude');
ylim([-0.2, 1.2]);
end