package test.jbi.integration.test.framework;

public class InstallerServiceFactory {
	
	private static final String OPENESB = "openesb";
	private static final String FUJI = "fuji";
	private static final String TEST_ENV = "TestEnv";

	synchronized public static InstallerService getInstallerService() throws Exception{
		String env = Configuration.getProperty(TEST_ENV);
		InstallerService result = null;
		if(env == null || env.trim().equals(OPENESB)){
			result = new OpenESBInstaller(Configuration
						.getJbiHost(), Configuration.getJbiPort(), Configuration
						.getJbiUsername(), Configuration.getJbiPassword(),
						Configuration.getJbiTarget());
		}else if(env.trim().equals(FUJI)){
			result = new FujiInstallerService(Configuration
						.getJMXServiceURL(), Configuration.getJbiUsername(),
						Configuration.getJbiPassword());
		
		}else{
			throw new Exception("Invalid environment");
		}
		return result;
	}

}
