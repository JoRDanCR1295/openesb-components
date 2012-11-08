package examples;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Vector;
import java.util.concurrent.Semaphore;


import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.ftpbc.connection.ConnectionPool;
import com.sun.jbi.ftpbc.ftp.FtpFileClient;
import com.sun.jbi.ftpbc.ftp.FtpFileConfigConstants;
import com.sun.jbi.ftpbc.ftp.FtpFileProvider;
import com.sun.jbi.ftpbc.ftp.FtpInterface;
import com.sun.jbi.ftpbc.ftp.connection.FTPBCConnectionManager;

public class TestFTPOverSSL implements Runnable {
    private static final String TRANS_MODE_ASCII="Ascii";
    private static final String TRANS_MODE_EBCDIC="Ebcdic";
    private static final String TRANS_MODE_BINARY="Binary";
    private String mThreadName;
    private Semaphore mSema;

    private String server = "jfu-tecra.stc.com";
    private String username = "user1";
    private String password = "password";
    private String mTargetDir;
    
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Semaphore sema1 = new Semaphore(1);
		List threads = new Vector();
		Thread t = null;
		for (int i = 0; i < 1; i++ ) {
			TestFTPOverSSL target = new TestFTPOverSSL();
			target.setTargetDir("BATCH_FTP_TEST_100");
				
			threads.add(t = new Thread(target, "UPLOADER" + i));
			target.setThreadName(t.getName());
			target.setSema(sema1);
		}

		Iterator it = threads.iterator();
		
		while ( it.hasNext() ) {
			((Thread)it.next()).start();
		}
		
		it = threads.iterator();

		boolean allDone = false;
		while ( threads.size() > 0 ) {
			Iterator itt = threads.iterator();
			while ( itt.hasNext()) {
				Thread tt = (Thread)itt.next();
				if ( !tt.isAlive() ) {
					threads.remove(tt);
					break;
				}
				try {
					Thread.currentThread().sleep(1000);
				}
				catch (Exception e) {
					// ignore
				}
			}
		}

//		FTPClient ftp = new FTPClient();
//		int port = 21;
//        String server = "jfu-tecra";
//        String username = "user1";
//        String password = "password";
//        String local = "C:\\TEMP\\TEST_DATA.txt";
//        String remote = "BATCH_FTP_DIR/data_test.txt";
//        String local = "C:\\TEMP\\TEST_DATA.TXT";
//        String remote = "BATCH_FTP_DIR/TEST_DATA.TXT";
//        String kstore = "C:\\KEY_AND_TRUST_STORES\\keystore.jks";
//        String tstore = "C:\\KEY_AND_TRUST_STORES\\keystore.jks";
//        String kstorepass = "password";
//        boolean storeFile = true;
//        boolean cccEnabled = false;
//        boolean ascii = false;
//        boolean passive = false;
        
//        try
//        {
//            Properties prop = new Properties();
//            prop.put(FtpFileConfigConstants.P_COLLAB_OID, "placeholder");
//            prop.put(FtpFileConfigConstants.P_CONN_NAME, "placeholder");
//            // section "General Settings"
//            prop.put(FtpFileConfigConstants.P_GEN_TRNS_TYPE, "Non-Transactional");
//            prop.put(FtpFileConfigConstants.P_GEN_BASE_LOC, "");
//            prop.put(FtpFileConfigConstants.C_P_GEN_SYNC, "No");
//            // section "FTP"
//            prop.put(FtpFileConfigConstants.C_P_FTP_LST_STYLE, "UNIX");
//            prop.put(FtpFileConfigConstants.C_P_FTP_HOST, server);
//            prop.put(FtpFileConfigConstants.C_P_FTP_USR, username);
//            prop.put(FtpFileConfigConstants.C_P_FTP_PASSWD, password);
//            prop.put(FtpFileConfigConstants.C_P_FTP_UDH_CFG, "");
//            prop.put(FtpFileConfigConstants.C_P_FTP_UDH_LST_STYLE, "");
//
//            prop.put(FtpFileConfigConstants.C_P_FTP_PORT, new Long(21));
//            
//            prop.put(FtpFileConfigConstants.P_FTP_MODE, TRANS_MODE_ASCII);
//            
//            prop.put(FtpFileConfigConstants.C_P_SECURE_FTP_TYPE, "None");
//            prop.put(FtpFileConfigConstants.C_P_ENABLE_CCC, "false");
//            prop.put(FtpFileConfigConstants.C_P_KEY_STORE_LOC, "");
//            prop.put(FtpFileConfigConstants.C_P_KEY_STORE_PASSWORD, "");
//            prop.put(FtpFileConfigConstants.C_P_KEY_ALIAS, "");
//            prop.put(FtpFileConfigConstants.C_P_KEY_PASSWORD, "");
//            prop.put(FtpFileConfigConstants.C_P_TRUST_STORE_LOC, "");
//            prop.put(FtpFileConfigConstants.C_P_TRUST_STORE_PASSWORD, "");
//            
//            prop.put(FtpFileConfigConstants.P_FTP_CMD_TIMEOUT, new Long(45000));
//            prop.put(FtpFileConfigConstants.P_FTP_DAT_TIMEOUT, new Long(45000));
//            
//            prop.put(FtpFileConfigConstants.C_P_SOC_ON, "No");
//            prop.put(FtpFileConfigConstants.C_P_SOC_HOST, "");
//            prop.put(FtpFileConfigConstants.C_P_SOC_PORT, new Long(1080));
//            prop.put(FtpFileConfigConstants.C_P_SOC_USR, "");
//            prop.put(FtpFileConfigConstants.C_P_SOC_PASSWD, "");
//            prop.put(FtpFileConfigConstants.C_P_SOC_VER, "Unknown");
//            
//            prop.put(FtpFileConfigConstants.P_STAGE_ENABLED,  "No");
//            
//            prop.put(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON, "Yes");
//            
//            prop.put(FtpFileConfigConstants.P_TGT_DIR, "BATCH_FTP_DIR");
//            prop.put(FtpFileConfigConstants.P_TGT_DIR_PATT, "No");
//            prop.put(FtpFileConfigConstants.P_TGT_FILE, "TARGET.TXT");
//            prop.put(FtpFileConfigConstants.P_TGT_FILE_PATT, "No");
//            prop.put(FtpFileConfigConstants.P_TGT_APPND, "No");
//            
//            prop.put(FtpFileConfigConstants.P_PRE_CMD, "None");
//            
//            prop.put(FtpFileConfigConstants.P_POST_CMD, "None");
//            
//            // section "FTP Raw Commands"
//            prop.put(FtpFileConfigConstants.P_RAW_PRE_CMD, "");
//            prop.put(FtpFileConfigConstants.P_RAW_POST_CMD, "");
//            
//            // section "Sequence Numbering"
//            prop.put(FtpFileConfigConstants.P_SEQ_START, new Long(1));
//            prop.put(FtpFileConfigConstants.P_SEQ_MAX, new Long(999999));
//            
//            prop.put(FtpFileConfigConstants.P_EXTENSION_PROVIDER_CLAZZ, "com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl");
//            prop.put(FtpFileConfigConstants.P_EXTENSION_CLIENT_CLAZZ, "com.sun.jbi.ftpbc.ftp.FtpFileClientImpl");
//            
//            prop.put(FtpFileConfigConstants.C_P_GEN_CONN_MODE, "Manual");
//            
//            // no retry
//            prop.put(FtpFileConfigConstants.CONNRETRY_MAXRETRIES, new Long(0));
//            prop.put(FtpFileConfigConstants.CONNRETRY_INTERVAL, new Long(1000));
//
//            int reply;
//            
//            int count = 10;
//            String targetFile = "TARGET.TXT";
//            
//            while ( count > 0 ) {
//                Connection connection = FTPBCConnectionManager.getConnection(prop);
//                
//                FtpInterface intf = (FtpInterface)connection.getClientObject();
//                
//                FtpFileClient client = intf.getClient();
//                FtpFileProvider provider = intf.getProvider();
//                client.setWarningOff(true);
//                provider.setWarningOff(true);
//                count--;
//                try {
//                    if ( !client.isConnected() )
//                        client.connect();
//                    client.setPayload(("PAYLOAD" + count).getBytes());
//                    client.put();
//                } catch (Exception e) {
//                	e.printStackTrace();
//                } finally {
//                    FTPBCConnectionManager.returnConnection(connection);
//                }
//                prop.put(FtpFileConfigConstants.P_TGT_FILE, targetFile + count);
//            }
//            
//            System.out.println("=======================================");
            
//        	ftp.setTrustStoreLoc(tstore);
//        	ftp.setTrustStorePassword(kstorepass);
//        	ftp.setKeyStoreLoc(kstore);
//        	ftp.setKeyStorePassword(kstorepass);
//        	
//        	ftp.setFTPType(FTPType.REGULAR_FTP);
        	//ftp.setFTPType(FTPType.FTP_OVER_EXPLICIT_SSL);
        	//ftp.setFTPType(FTPType.FTP_OVER_IMPLICIT_SSL);

        	
        	
        	
//        	ftp.connect(server, port);
//            
//        	System.out.println("Connected to " + server + ".");
//
//            // After connection attempt, you should check the reply code to verify
//            // success.
//            reply = ftp.getReplyCode();
//
//            if (!FTPReply.isPositiveCompletion(reply))
//            {
//                ftp.disconnect();
//                System.err.println("FTP server refused connection.");
//                System.exit(1);
//            }
//        }
//        catch (Exception e)
//        {
//            if (ftp.isConnected())
//            {
//                try
//                {
//                    ftp.disconnect();
//                }
//                catch (IOException f)
//                {
//                    // do nothing
//                }
//            }
//            System.err.println("Could not connect to server.");
//            e.printStackTrace();
//            System.exit(1);
//        }
//
//        try
//        {
//            if (!ftp.login(username, password))
//            {
//                ftp.logout();
//            }
//
//            if ( cccEnabled )
//            	ftp.doCCC();
//            
//            System.out.println("Remote system is " + ftp.getSystemName());
//
//        	if ( passive )
//            	ftp.enterLocalPassiveMode();
//
//            // list the dir before doing anything
//            FTPFile[] files = ftp.listFiles() ;
//            
//            for (int i = 0; i <files.length; i++ ) {
//            	System.out.println("File[" + i + "]=" + files[i].getName());
//            }
//
//        	ftp.setFileType(ascii ? ftp.ASCII_FILE_TYPE : ftp.BINARY_FILE_TYPE );
//
//            if (storeFile)
//            {
//                InputStream input;
//                System.out.println("BEFORE store local : " + local + " ====> remote: " + remote);
//                input = new FileInputStream(local);
//                OutputStream output = ftp.storeFileStream(remote);
//                int len = 0;
//                byte[] buffer = new byte[1024];
//                while ( (len = input.read(buffer)) > 0 ) {
//                	output.write(buffer, 0, len);
//                	output.flush();
//                }
////                ftp.storeFile(remote, input);
//                output.close();
//                System.out.println("AFTER store local : " + local + " ====> remote: " + remote);
//            }
//            else
//            {
//                OutputStream output;
//                System.out.println("BEFORE retrieve from remote: " + remote + " ===> local: " + local);
//                output = new FileOutputStream(local);
//                ftp.retrieveFile(remote, output);
////                InputStream src = ftp.retrieveFileStream(remote);
////                int l = 0;
////                byte[] buffer = new byte[1024];
////                while ( (l = src.read(buffer)) > 0 ) {
////                	System.out.print(new String(buffer, 0, l));
////                }
////                src.close();
//                System.out.println("AFTER retrieve from remote: " + remote + " ===> local: " + local);
//            }
//
//            ftp.logout();
//            System.out.println("LOGOUT ============");
//        }
//        catch (Exception e) {
//        	e.printStackTrace();
//        }
//        finally
//        {
//            if (ftp.isConnected())
//            {
//                try
//                {
//                    ftp.disconnect();
//                }
//                catch (IOException f)
//                {
//                    // do nothing
//                }
//            }
//        }
	}
	
	public void doUpload() {
      try
      {
          Properties prop = new Properties();
          prop.put(FtpFileConfigConstants.P_COLLAB_OID, "placeholder");
          prop.put(FtpFileConfigConstants.P_CONN_NAME, "placeholder");
          // section "General Settings"
          prop.put(FtpFileConfigConstants.P_GEN_TRNS_TYPE, "Non-Transactional");
          prop.put(FtpFileConfigConstants.P_GEN_BASE_LOC, "");
          prop.put(FtpFileConfigConstants.C_P_GEN_SYNC, "No");
          // section "FTP"
          prop.put(FtpFileConfigConstants.C_P_FTP_LST_STYLE, "UNIX");
          prop.put(FtpFileConfigConstants.C_P_FTP_HOST, server);
          prop.put(FtpFileConfigConstants.C_P_FTP_USR, username);
          prop.put(FtpFileConfigConstants.C_P_FTP_PASSWD, password);
          prop.put(FtpFileConfigConstants.C_P_FTP_UDH_CFG, "");
          prop.put(FtpFileConfigConstants.C_P_FTP_UDH_LST_STYLE, "");

          prop.put(FtpFileConfigConstants.C_P_FTP_PORT, new Long(21));
          
          prop.put(FtpFileConfigConstants.P_FTP_MODE, TRANS_MODE_ASCII);
          
          prop.put(FtpFileConfigConstants.C_P_SECURE_FTP_TYPE, "ExplicitSSL");
          prop.put(FtpFileConfigConstants.C_P_ENABLE_CCC, "false");
          prop.put(FtpFileConfigConstants.C_P_KEY_STORE_LOC, "C:/KEY_AND_TRUST_STORES/keystore.jks");
          prop.put(FtpFileConfigConstants.C_P_KEY_STORE_PASSWORD, "password");
          prop.put(FtpFileConfigConstants.C_P_KEY_ALIAS, "");
          prop.put(FtpFileConfigConstants.C_P_KEY_PASSWORD, "");
          prop.put(FtpFileConfigConstants.C_P_TRUST_STORE_LOC, "C:/KEY_AND_TRUST_STORES/keystore.jks");
          prop.put(FtpFileConfigConstants.C_P_TRUST_STORE_PASSWORD, "password");
          
          prop.put(FtpFileConfigConstants.P_FTP_CMD_TIMEOUT, new Long(45000));
          prop.put(FtpFileConfigConstants.P_FTP_DAT_TIMEOUT, new Long(45000));
          
          prop.put(FtpFileConfigConstants.C_P_SOC_ON, "No");
          prop.put(FtpFileConfigConstants.C_P_SOC_HOST, "");
          prop.put(FtpFileConfigConstants.C_P_SOC_PORT, new Long(1080));
          prop.put(FtpFileConfigConstants.C_P_SOC_USR, "");
          prop.put(FtpFileConfigConstants.C_P_SOC_PASSWD, "");
          prop.put(FtpFileConfigConstants.C_P_SOC_VER, "Unknown");
          
          prop.put(FtpFileConfigConstants.P_STAGE_ENABLED,  "No");
          
          prop.put(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON, "Yes");
          
          prop.put(FtpFileConfigConstants.P_TGT_DIR, mTargetDir);
          prop.put(FtpFileConfigConstants.P_TGT_DIR_PATT, "No");
          prop.put(FtpFileConfigConstants.P_TGT_FILE, "TARGET.TXT");
          prop.put(FtpFileConfigConstants.P_TGT_FILE_PATT, "No");
          prop.put(FtpFileConfigConstants.P_TGT_APPND, "No");
          
          prop.put(FtpFileConfigConstants.P_PRE_CMD, "None");
          
          prop.put(FtpFileConfigConstants.P_POST_CMD, "None");
          
          // section "FTP Raw Commands"
          prop.put(FtpFileConfigConstants.P_RAW_PRE_CMD, "");
          prop.put(FtpFileConfigConstants.P_RAW_POST_CMD, "");
          
          // section "Sequence Numbering"
          prop.put(FtpFileConfigConstants.P_SEQ_START, new Long(1));
          prop.put(FtpFileConfigConstants.P_SEQ_MAX, new Long(999999));
          
          prop.put(FtpFileConfigConstants.P_EXTENSION_PROVIDER_CLAZZ, "com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl");
          prop.put(FtpFileConfigConstants.P_EXTENSION_CLIENT_CLAZZ, "com.sun.jbi.ftpbc.ftp.FtpFileClientImpl");
          
          prop.put(FtpFileConfigConstants.C_P_GEN_CONN_MODE, "Manual");
          
          // no retry
          prop.put(FtpFileConfigConstants.CONNRETRY_MAXRETRIES, new Long(0));
          prop.put(FtpFileConfigConstants.CONNRETRY_INTERVAL, new Long(1000));

          // connection pool params
          prop.put(ConnectionPool.POOL_MIN_SIZE, new Integer(2));
          prop.put(ConnectionPool.POOL_MAX_SIZE, new Integer(32));
          prop.put(Connection.CONN_MAX_IDEL_TIMEOUT, new Integer(60000));

          int reply;
          
          int count = 10;
          String targetFile = "TARGET.TXT";
          
          while ( count > 0 ) {
              prop.put(FtpFileConfigConstants.P_TGT_FILE, mThreadName + "_" + targetFile + "_" + count);
              Connection connection = FTPBCConnectionManager.getConnection(prop);
              System.out.println("===================" + mThreadName + "==================== GET connection :" + connection);
              FtpInterface intf = (FtpInterface)connection.getClientObject();
              
              FtpFileClient client = intf.getClient();
              FtpFileProvider provider = intf.getProvider();
              client.setWarningOff(true);
              provider.setWarningOff(true);
              count--;
              try {
                  mSema.acquire();
                  if ( !client.isConnected() )
                      client.connect();
                  //provider.cd(""); // go home
                  //client.setPayload(("PAYLOAD" + count).getBytes());
                  //client.put();
                  client.get();
                  File f = new File("c:/temp", "download_data_" + count + ".txt");
                  FileOutputStream fos = new FileOutputStream(f);
                  String payload = new String(client.getPayload());
                  fos.write(payload.getBytes());
                  fos.close();
              } catch (Exception e) {
              	e.printStackTrace();
              	if ( connection != null ) {
              		connection.discard();
              		connection = null;
              	}
              } finally {
            	  mSema.release();
            	  if ( connection != null ) {
            		  try {
                          client.disconnect();
            		  }
            		  catch (Exception e) {
            			  // ignore
            		  }
	                  FTPBCConnectionManager.returnConnection(connection);
	                  System.out.println("===================" + mThreadName + "==================== RETURN connection :" + connection);
            	  }
              }
          }
          
          System.out.println("===================" + mThreadName + "====================");
      }
      catch (Exception e)
      {
    	  e.printStackTrace();
      }
	}

	public void setThreadName(String name) {
		mThreadName = name;
	}
	
	public void setSema(Semaphore sema) {
		mSema = sema;
	}
	
	public Semaphore getSema() {
		return mSema;
	}

	public void setTargetDir(String dir) {
		mTargetDir = dir;
	}
	
	public void run() {
		// TODO Auto-generated method stub
		doUpload();
	}
}
