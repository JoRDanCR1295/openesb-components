package test.jbi.integration.test.framework;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import javax.jbi.component.ComponentContext;

import junit.framework.TestCase;
import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.core.Connection;
import test.jbi.integration.testbc.core.SandboxClassLoader;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.integration.testbc.installer.TestBCInstaller;

import com.sun.jbi.ui.common.JBIRemoteException;

abstract public class IntegrationTestCase extends TestCase implements Serializable{
	
	private static String mTestBC;
	private static InstallerService mJbiInstaller; 
	private static Connection mCon;
	private static boolean initialized = false;

	private static final boolean REMOTE;
	static{
		if(Configuration.class.getClassLoader().getClass().getName().equals(SandboxClassLoader.class.getName())){
			REMOTE = true;
		}else{
			REMOTE = false;
			Runtime.getRuntime().addShutdownHook(new Thread() {
				@Override
				public void run() {
					try {
						cleanup();
					} catch (Exception e) {}
				}
			});
		}
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
	
		if(!initialized){
			createInstaller();

			String testBCDir = TestBCInstaller.generateInstaller(Configuration
					.getWorkingDir());
			
			for(int i=0;;++i){
				try{
					//make sure it is uninstalled
					mJbiInstaller.uninstallComponent("TestBc");
				}catch(Exception ex){}

				try{
					mTestBC = mJbiInstaller.installComponent(testBCDir);
					mJbiInstaller.startComponent(mTestBC);
					break;
				}catch(Throwable t){
					if(i==10)
						throw new Exception("Could not install testBC", t);
					Thread.sleep(10000);
				}
				
			}
			
			mCon = new Connection(Configuration.getJbiHost(), Configuration.getTestBCPort());
			mCon.start();
			initialized = true;
		}
	}

	private void createInstaller() throws Exception {
//		mJbiInstaller = new OpenESBInstaller(Configuration
//				.getJbiHost(), Configuration.getJbiPort(), Configuration
//				.getJbiUsername(), Configuration.getJbiPassword(),
//				Configuration.getJbiTarget());
		mJbiInstaller = InstallerServiceFactory.getInstallerService();
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	static private void cleanup() throws Exception {
		if(REMOTE){
			if(mJbiInstaller != null){
				mJbiInstaller.close();
			}
		}else{
			mCon.close();
			mJbiInstaller.uninstallComponent("TestBc");
			mJbiInstaller.close();
		}
	}
	
	protected InstallerService getInstaller() throws Exception{
		if(mJbiInstaller == null)
			createInstaller();
		return mJbiInstaller;
	}
	
	protected Connection getConnection(){
		return mCon;
	}
	
	@Override
	protected void runTest() throws Throwable {
		final String methodName = getName();
		if(methodName.startsWith("test_Integration")){
			try{
				getConnection().execute(new Command() {
					public Serializable execute(ComponentContext context) throws Exception {
						invoke(methodName);
						return "Success";
					}
				});
			}catch(Throwable e){
				if(e.getCause()==null)
					throw e;
				throw e.getCause();
			}
		}else{
			invoke(methodName);
		}
	}
	
	private void invoke(String name) throws Exception {
		try{
			Method runMethod = getClass().getMethod(name, (Class[])null);
			runMethod.invoke(this);
			
		}catch(InvocationTargetException e){
			throw new Exception("Root Cause", e.getCause());
		}catch(Throwable e){
			throw new Exception("Root Cause", e);
		}
	}
	 
}
