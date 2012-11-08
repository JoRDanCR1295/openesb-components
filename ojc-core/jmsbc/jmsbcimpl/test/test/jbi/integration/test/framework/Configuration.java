package test.jbi.integration.test.framework;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import test.jbi.integration.testbc.core.SandboxClassLoader;
import test.jbi.integration.testbc.impl.JbiHelper;

final public class Configuration {
	
	private static final String sWorkingDir;
	private static final String sJbiHost;
	private static final int sJbiPort;
	private static final String sJbiUsername;
	private static final String sJbiPassword;
	private static final String sJbiTarget;
	private static final String sJMXServiceURL;
	private static final int sTestBCPort;
	private static final Properties sProp = new Properties();

	
	static{
	      InputStream in = Configuration.class.getResourceAsStream("config.properties");
       if(in!=null){
			try {
				sProp.load(in);
			} catch (IOException e) {
				e.printStackTrace();
			}
       }
		if(Configuration.class.getClassLoader().getClass().getName().equals(SandboxClassLoader.class.getName())){
			sProp.put("WorkingDir", JbiHelper.getComponentContext().getWorkspaceRoot());
		}
		sWorkingDir = sProp.getProperty("WorkingDir", System.getProperty("java.io.tmpdir"));
		sJbiHost = sProp.getProperty("JbiHost", "localhost");

		int port = 8686;
		try{
			port = Integer.parseInt(sProp.getProperty("JbiPort", "8686"));
		}catch(Throwable t){
		}
		sJbiPort = port;
		
		sJbiUsername = sProp.getProperty("JbiUsername", "admin");
		sJbiPassword = sProp.getProperty("JbiPassword", "adminadmin");
		sJbiTarget = sProp.getProperty("JbiTarget", "server");
		sJMXServiceURL = sProp.getProperty("JMXServiceURL", "service:jmx:rmi:///jndi/rmi://:8686/jmxrmi");
		
		port = 9888;
		try{
			port = Integer.parseInt(sProp.getProperty("TestBCPort", "9888"));
		}catch(Throwable t){
		}
		sTestBCPort = port;
		
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
	
	public static int getTestBCPort(){
		return sTestBCPort;
	}
	
	public static String getProperty(String name){
		return sProp.getProperty(name);
	}
}
