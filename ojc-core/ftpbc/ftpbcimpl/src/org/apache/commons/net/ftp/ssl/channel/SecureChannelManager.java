package org.apache.commons.net.ftp.ssl.channel;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

public class SecureChannelManager {
	private final static Logger log = Logger.getLogger("SecureChannelManager");
		
	private final Set readListeners = new HashSet();
	private final Set writeListeners = new HashSet();

	/**
	 * Called by the SelectorThread to give the SecureChannels
	 * a chance to fire the events to its listeners
	 */
	public void fireEvents() {
		while (!readListeners.isEmpty() || !writeListeners.isEmpty()) {
			SecureChannel[] sc;
			if (!readListeners.isEmpty()) {
				sc = (SecureChannel[])readListeners.toArray(
						new SecureChannel[readListeners.size()]);
				readListeners.clear();
				for (int i = 0; i < sc.length; i++) {
					sc[i].fireReadEvent();
				}
			}
			
			if (!writeListeners.isEmpty()) {
				sc = (SecureChannel[])writeListeners.toArray(
						new SecureChannel[writeListeners.size()]);
				writeListeners.clear();
				for (int i = 0; i < sc.length; i++) {
					sc[i].fireWriteEvent();
				}
			}
		}
	}
		
	public void registerForRead(SecureChannel l) {
		boolean wasNotPresent = readListeners.add(l);
	}
	
	public void unregisterForRead(SecureChannel l) {
		boolean wasPresent = readListeners.remove(l);
	}
		
	public void registerForWrite(SecureChannel l) {
		boolean wasNotPresent = writeListeners.add(l);
	}
	
	public void unregisterForWrite(SecureChannel l) {
		boolean wasPresent = writeListeners.remove(l);
	}
}
