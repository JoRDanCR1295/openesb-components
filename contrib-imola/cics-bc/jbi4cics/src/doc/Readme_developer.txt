Passi per partire con il progetto

-- istallare eclipse 3.1 http://www.eclipse.org
-- istallare maven plugin per eclipse http://mevenide.codehaus.org/
-- istallare maven 1.0.2 http://maven.apache.org/
-- istallare il subversion plugin per eclipse http://subclipse.tigris.org/

-- copiare il file $MAVEN_HOME/bin/forehead.conf in $ECLIPSE_HOME/bin
-- nella vista svn repository configurare il repository (dipende dall'ambiente)

-- scaricare il progetto svn://<ip>/iif/trunk
-- configurare le properties di maven location in eclipse:
	maven.local.home= a scelta, il default è $HOME/.maven, io consiglio di creare una direcotry local in $MAVEN_HOME quindi diventa--> $MAVEN_HOME/local 
	maven.local.repository=${maven.local.home}/repository

-- copiare il file $IIF_HOME/main/src/etc/maven/build.properties in  (non so in windows...)
-- nel file $HOME/build.properties
	 disabilitare le properies (la modalità offline è obbligatoria dietro il proxy bpm, se si imposta il proxy i jar scaricati vengono corrotti)
   	maven.repo.remote.enabled
   	maven.mode.online
   impostare correttamente
   maven.local.home= vedi sopra
   maven.home
   
-- applicare le patch di maven che si trovano in $IIF_HOME/main/lib/maven alcune patch si possono applicare solo dopo avere scaricato il relativo plugin
	le patch vanno applicate (cioè copiare il file) in ${maven.local.repository}/cache e poi la directory del plugin da patchare
    
-- per controllare la corretta istallazione provare il target maven site:generate 
