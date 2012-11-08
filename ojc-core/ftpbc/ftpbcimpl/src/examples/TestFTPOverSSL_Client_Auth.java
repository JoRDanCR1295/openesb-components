package examples;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;
import java.util.concurrent.Semaphore;

import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPReply;
import org.apache.commons.net.ftp.FTP.FTPType;

import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.ftpbc.connection.ConnectionPool;
import com.sun.jbi.ftpbc.ftp.FtpFileClient;
import com.sun.jbi.ftpbc.ftp.FtpFileConfigConstants;
import com.sun.jbi.ftpbc.ftp.FtpFileProvider;
import com.sun.jbi.ftpbc.ftp.FtpInterface;
import com.sun.jbi.ftpbc.ftp.connection.FTPBCConnectionManager;


public class TestFTPOverSSL_Client_Auth {
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
		FTPClient ftp = new FTPClient();
		int port = 21;
        String server = "jfu-tecra";
        String username = "user1";
        String password = "password";
        String local = "C:\\TEMP\\TEST_DATA.txt";
        String remote = "BATCH_FTP_DIR/data_test.txt";
        String kstore = "C:\\KEY_AND_TRUST_STORES\\keystore.jks";
        String tstore = "C:\\KEY_AND_TRUST_STORES\\keystore.jks";
        String kstorepass = "password";
        boolean storeFile = false;
        boolean cccEnabled = false;
        boolean ascii = false;
        boolean passive = false;
        
        int reply;
        
        String targetFile = "TARGET.TXT";
        
    	
        try
        {
            ftp.setTrustStoreLoc(tstore);
            ftp.setTrustStorePassword(kstorepass);
            ftp.setKeyStoreLoc(kstore);
            ftp.setKeyStorePassword(kstorepass);
            ftp.setClientAlias("jfu");
            ftp.setAliasPass("12345678");
        	//ftp.setFTPType(FTPType.REGULAR_FTP);
        	ftp.setFTPType(FTPType.FTP_OVER_EXPLICIT_SSL);
        	//ftp.setFTPType(FTPType.FTP_OVER_IMPLICIT_SSL);

        	ftp.connect(server, port);
            
        	System.out.println("Connected to " + server + ".");
        }
        catch (Exception e)
        {
            if (ftp.isConnected())
            {
                try
                {
                    ftp.disconnect();
                }
                catch (IOException f)
                {
                    // do nothing
                }
            }
            System.err.println("Could not connect to server.");
            e.printStackTrace();
            System.exit(1);
        }

        try
        {
            if (!ftp.login(username, password))
            {
                ftp.logout();
            }

            if ( cccEnabled )
            	ftp.doCCC();
            
            System.out.println("Remote system is " + ftp.getSystemName());

        	if ( passive )
            	ftp.enterLocalPassiveMode();

            // list the dir before doing anything
            FTPFile[] files = ftp.listFiles() ;
            
            for (int i = 0; i <files.length; i++ ) {
            	System.out.println("File[" + i + "]=" + files[i].getName());
            }

        	ftp.setFileType(ascii ? ftp.ASCII_FILE_TYPE : ftp.BINARY_FILE_TYPE );

            if (storeFile)
            {
                InputStream input;
                System.out.println("BEFORE store local : " + local + " ====> remote: " + remote);
                input = new FileInputStream(local);
                OutputStream output = ftp.storeFileStream(remote);
                int len = 0;
                byte[] buffer = new byte[1024];
                while ( (len = input.read(buffer)) > 0 ) {
                	output.write(buffer, 0, len);
                	output.flush();
                }
                //ftp.storeFile(remote, input);
                output.close();
                System.out.println("AFTER store local : " + local + " ====> remote: " + remote);
            }
            else
            {
                OutputStream output;
                System.out.println("BEFORE retrieve from remote: " + remote + " ===> local: " + local);
                output = new FileOutputStream(local);
                ftp.retrieveFile(remote, output);
//                InputStream src = ftp.retrieveFileStream(remote);
//                int l = 0;
//                byte[] buffer = new byte[1024];
//                while ( (l = src.read(buffer)) > 0 ) {
//                	System.out.print(new String(buffer, 0, l));
//                }
//                src.close();
                System.out.println("AFTER retrieve from remote: " + remote + " ===> local: " + local);
            }

            ftp.logout();
            System.out.println("LOGOUT ============");
        }
        catch (Exception e) {
        	e.printStackTrace();
        }
        finally
        {
            if (ftp.isConnected())
            {
                try
                {
                    ftp.disconnect();
                }
                catch (IOException f)
                {
                    // do nothing
                }
            }
        }
	}
}
