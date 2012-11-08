package org.apache.commons.net.ftp.ssl;

import java.net.Socket;
import java.security.Key;
import java.security.KeyStore;
import java.security.Principal;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;

import javax.net.ssl.SSLEngine;
import javax.net.ssl.X509ExtendedKeyManager;
import javax.net.ssl.X509KeyManager;

public class SelectableAliasX509ExtendedKeyManager extends
		X509ExtendedKeyManager {

	private X509KeyManager mManager;
	private String mAlias;
	private String mAliasPass;
	private KeyStore mKeyStore;
	
	private SelectableAliasX509ExtendedKeyManager() {
	}

	public static SelectableAliasX509ExtendedKeyManager createKeyManager(X509KeyManager manager, KeyStore kstore, String alias, String aliasPass) {
		SelectableAliasX509ExtendedKeyManager selectedAliasManager = new SelectableAliasX509ExtendedKeyManager();
		selectedAliasManager.setAlias(alias);
		selectedAliasManager.setAliasPass(aliasPass);
		selectedAliasManager.setDefaultManager(manager);
		selectedAliasManager.setKeyStore(kstore);
		return selectedAliasManager;
	}
	
	public void setDefaultManager(X509KeyManager manager) {
		mManager = manager;
	}
	
	public void setAlias(String alias) {
		mAlias = alias;
	}
	
	public void setAliasPass(String aliasPass) {
		mAliasPass = aliasPass;
	}
	
	public void setKeyStore(KeyStore kstore) {
		mKeyStore = kstore;
	}
	
	public String chooseClientAlias(String[] keyType, Principal[] issuers, Socket socket) {
		return mManager.chooseClientAlias(keyType, issuers, socket);
	}

	public String chooseServerAlias(String keyType, Principal[] issuers, Socket socket) {
		return mManager.chooseServerAlias(keyType, issuers, socket);
	}

	public X509Certificate[] getCertificateChain(String alias) {
		return mManager.getCertificateChain(alias);
	}

	public String[] getClientAliases(String keyType, Principal[] issuers) {
		return mManager.getClientAliases(keyType, issuers);
	}

	public PrivateKey getPrivateKey(String alias) {
		if ( alias != null && alias.equals(mAlias) && mAliasPass != null ) {
			
			Key k = null;
			try {
				k = mKeyStore.getKey(mAlias, mAliasPass.toCharArray());
			}
			catch (Exception e) {
				// return null key
				e.printStackTrace();
			}
			if ( k instanceof PrivateKey ) {
				return (PrivateKey)k;
			}
			else {
				throw new IllegalArgumentException("Alias " + alias + " not associated with a private key.");
			}
		}
		else {
			return mManager.getPrivateKey(alias);
		}
	}

	public String[] getServerAliases(String keyType, Principal[] issuers) {
		return mManager.getServerAliases(keyType, issuers);
	}

	public String chooseEngineClientAlias(String[] keyType, Principal[] issuers, SSLEngine engine) {
		if ( mAlias != null && mAlias.trim().length() > 0 ) {
			return mAlias;
		}
		else {
			return super.chooseEngineClientAlias(keyType, issuers, engine);
			
		}
	}
	
	public String chooseEngineServerAlias(String keyType, Principal[] issuers, SSLEngine engine) {
		return super.chooseEngineServerAlias(keyType, issuers, engine);
	}
}
