package test.jbi.integration.test.framework;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

final public class Configuration {
	
	private static final String sWorkingDir;
	private static final String sJbiHost;
	private static final int sJbiPort;
	private static final String sJbiUsername;
	private static final String sJbiPassword;
	private static final String sJbiTarget;
	private static final String sJMXServiceURL;
	
	
	static{

		InputStream in = Configuration.class.getResourceAsStream("config.properties");
		Properties prop = new Properties();
		if(in!=null){
			try {
				prop.load(in);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		sWorkingDir = prop.getProperty("WorkingDir", System.getProperty("java.io.tmpdir"));
		sJbiHost = prop.getProperty("JbiHost", "localhost");

		int port = 8686;
		try{
			port = Integer.parseInt(prop.getProperty("JbiPort", "8686"));
		}catch(Throwable t){
		}
		sJbiPort = port;
		
		sJbiUsername = prop.getProperty("JbiUsername", "admin");
		sJbiPassword = prop.getProperty("JbiPassword", "adminadmin");
		sJbiTarget = prop.getProperty("JbiTarget", "server");
		sJMXServiceURL = prop.getProperty("JMXServiceURL", "service:jmx:rmi:///jndi/rmi://:8686/jmxrmi");
		
	}

	public static String getWorkingDir(){
		return sWorkingDir;
	}
	
	public static String getJbiHost(){
		return sJbiHost;
	}

	public static int getJbiPort(){
		return sJbiPort;
	}

	public static String getJbiUsername(){
		return sJbiUsername;
	}

	public static String getJbiPassword(){
		return sJbiPassword;
	}

	public static String getJbiTarget(){
		return sJbiTarget;
	}
	
	public static String getJMXServiceURL(){
		return sJMXServiceURL;
	}
	
	public static String getPath(Class cls, String fileName){
		//Latter we can change this code to test run integration tests with other JMS providers.
		//Right now the JMS providers are mentioned in the WSDL. In future we can check some system property 
		//to see which JMS provider we want to test and return the corresponding WSDLS files. May be we can 
		//append the file name with the JMS provider name.
		URL url = cls.getResource(fileName);
		if(url == null)
			return null;
		
		return url.getPath();
	}
}
