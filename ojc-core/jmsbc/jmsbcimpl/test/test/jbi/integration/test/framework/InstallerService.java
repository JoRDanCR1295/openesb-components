package test.jbi.integration.test.framework;



public interface InstallerService {

	public String deployServiceAssembly(String zipFileName) throws Exception;

	public void undeployServiceAssembly(String saName) throws Exception ;

	public boolean isJBIRuntimeEnabled() throws Exception;

	public String installComponent(String componentZipFile) throws Exception;

	public void uninstallComponent(String componentName) throws Exception;

	public void startComponent(String componentName) throws Exception;

	public void shutdownServiceAssembly(String saName) throws Exception;
	
	public void stopServiceAssembly(String saName) throws Exception;	

	public void startServiceAssembly(String saName) throws Exception;
	
	public void close();
	
}
