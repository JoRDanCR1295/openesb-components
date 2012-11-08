package test.jbi.integration.test.framework;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.TabularDataSupport;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

public class FujiInstallerService implements InstallerService {

	private static final String START_BUNDLE = "startBundle";
	private static final String INSTALL_BUNDLE = "installBundle";
	private static final String[] STRING_SIGNATURE = new String[]{String.class.getName()};
	private static final String[] LONG_SIGNATURE = new String[]{long.class.getName()};
	private static final ObjectName OBJEXT_NAME_COMPONENT_INSTALLER;
	private static final ObjectName OBJEXT_NAME_SA_INSTALLER;
	
	static{
		try{
			OBJEXT_NAME_COMPONENT_INSTALLER = new ObjectName("con.sun.jbi.fuji:runtime=OSGi,type=admin,service=admin");
			OBJEXT_NAME_SA_INSTALLER = new ObjectName("com.sun.jbi:ControlType=DeploymentService,ServiceName=DeploymentService");
		}catch(MalformedObjectNameException ex){
			throw new RuntimeException(ex);
		}
	}
	
	private MBeanServerConnection mCon;
	private JMXConnector mJmxc;
	
	public FujiInstallerService(String jmxServiceURL, String userId, String password) throws Exception{
		String[] credentials = new String[]{userId, password};
		HashMap<String, Object> props = new HashMap<String, Object>();
		props.put("jmx.remote.credentials", credentials);
		props.put("java.naming.factory.initial", "com.sun.enterprise.naming.SerialInitContextFactory");
		
		JMXServiceURL url = new JMXServiceURL(jmxServiceURL);
		mJmxc = JMXConnectorFactory.connect(url, props);
		mCon = mJmxc.getMBeanServerConnection();
	}
	public String deployServiceAssembly(String zipFileName) throws Exception {
		return (String) mCon.invoke(OBJEXT_NAME_SA_INSTALLER, "deploy",
				new Object[] { "file:/" + zipFileName }, STRING_SIGNATURE);
	}

	public String installComponent(String componentZipFile)
			throws Exception {
		return (String) mCon.invoke(OBJEXT_NAME_COMPONENT_INSTALLER, INSTALL_BUNDLE,
				new Object[] { componentZipFile }, STRING_SIGNATURE);
	}

	public boolean isJBIRuntimeEnabled() throws Exception {
		return true;
	}

	public void shutdownServiceAssembly(String saName)
			throws Exception {
		mCon.invoke(OBJEXT_NAME_SA_INSTALLER, "shutDown",
				new Object[] { saName }, STRING_SIGNATURE);
	}

	public void startComponent(String componentName)
			throws Exception {
		mCon.invoke(OBJEXT_NAME_COMPONENT_INSTALLER, START_BUNDLE, new Object[] {Long.valueOf(componentName) },
				LONG_SIGNATURE);
	}
	
	private String findBundle(String componentName) throws Exception {
		String bundleID = null;
		TabularDataSupport table = (TabularDataSupport)mCon.invoke(OBJEXT_NAME_COMPONENT_INSTALLER, "listBundles", new Object[0], new String[0]);
		for(Object obj : table.entrySet()){
			CompositeDataSupport value = (CompositeDataSupport)((Map.Entry)obj).getValue();
			String symbolicName = (String)value.get("symbolicName");
			if(symbolicName != null && symbolicName.equals(componentName)){
				bundleID = (String)value.get("bundleID");
				break;
			}
		}
		if(bundleID == null)
			throw new Exception("Could not find bundle with name " + componentName);
		return bundleID;
	}

	public void startServiceAssembly(String saName) throws Exception {
		mCon.invoke(OBJEXT_NAME_SA_INSTALLER, "start",
				new Object[] { saName }, STRING_SIGNATURE);
	}

	public void stopServiceAssembly(String saName) throws Exception {
		mCon.invoke(OBJEXT_NAME_SA_INSTALLER, "stop",
				new Object[] { saName }, STRING_SIGNATURE);
	}

	public void undeployServiceAssembly(String saName)
			throws Exception {
		shutdownServiceAssembly(saName);
		mCon.invoke(OBJEXT_NAME_SA_INSTALLER, "undeploy",
				new Object[] { saName }, STRING_SIGNATURE);
	}

	public void uninstallComponent(String componentName)
			throws Exception {
		String bundleID = findBundle(componentName);
		mCon.invoke(OBJEXT_NAME_COMPONENT_INSTALLER, "stopBundle", new Object[] { Long
				.valueOf(bundleID) },
				LONG_SIGNATURE);
		mCon.invoke(OBJEXT_NAME_COMPONENT_INSTALLER, "uninstallBundle", new Object[] { Long
				.valueOf(bundleID) },
				LONG_SIGNATURE);
	}
	public void close() {
		try {
			mJmxc.close();
		} catch (IOException e) {}
	}

}
