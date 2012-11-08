/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovidertypedefany;

import it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoComplexTypeDefAny;
import it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoComplexTypeDefAnyHelper;
import it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoComplexTypeDefAnyPOA;
import it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoStruct;
import it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MySequence;

import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnySeqHelper;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

/**
 * This class is the corba servant used to manage the 'AnyTypes.idl'
 */
public class EchoComplexTypeDefAnyImpl extends EchoComplexTypeDefAnyPOA {

	/**
	 * logger.
	 */
	private static Logger log = Logger
			.getLogger(EchoComplexTypeDefAnyImpl.class.getName());
	// LogFactory.getLog(EchoImpl.class);

	static ORB orb = null;

	/**
	 * main first argument is the port (host is supposed to be localhost) second
	 * argument is daemon=true/false optional, default false. If daemon is true
	 * the servan starts as daemon, useful for integration tests
	 */
	public static void main(String[] args) throws Exception {
		String propertyFile = args[0];

		boolean daemon = args.length > 1 ? "daemon=true".equals(args[1])
				: false;
		startCorbaServant(daemon, propertyFile);

	}

	private static void startOrbd(final String port) {
		Thread orbdThread = new Thread(new Runnable() {

			public void run() {
				log.info("starting orbd on port: " + port);
				com.sun.corba.se.impl.activation.ORBD
						.main(new String[] { "-ORBInitialPort", port,
								"-ORBInitialHost", "localhost" });
			}
		});
		orbdThread.setDaemon(true);
		orbdThread.start();
		log.info("orbd launched");
	}

	private static void startCorbaServant(final boolean daemon,
			final String orbPropertyFile) {
		Thread servantThread = new Thread(new Runnable() {

			public void run() {
				try {
					// create and initialize the ORB
					log.info("loading orb.properties: " + orbPropertyFile);

					InputStream is = this.getClass().getResourceAsStream(
							"/" + orbPropertyFile);
					log.info("input stream: " + is);
					Properties props = new Properties();
					props.load(is);

					log.info("launching orb with properties: " + props);

					orb = ORB.init((String[]) null, props);

					// get reference to rootpoa & activate the POAManager
					POA rootpoa = POAHelper.narrow(orb
							.resolve_initial_references("RootPOA"));
					rootpoa.the_POAManager().activate();

					// create servant and register it with the ORB
					EchoComplexTypeDefAnyImpl helloImpl = new EchoComplexTypeDefAnyImpl();
					log.info("EchoImpl ..." + helloImpl);

					// get object reference from the servant
					org.omg.CORBA.Object ref = rootpoa
							.servant_to_reference(helloImpl);
					EchoComplexTypeDefAny href = EchoComplexTypeDefAnyHelper
							.narrow(ref);

					if (daemon) {
						startOrbd(props.getProperty("orbd.port"));
						Thread.currentThread().sleep(2000);
					}

					// get the root naming context
					// NameService invokes the name service
					org.omg.CORBA.Object objRef = orb
							.resolve_initial_references("NameService");

					// Use NamingContextExt which is part of the Interoperable
					// Naming Service (INS) specification.
					NamingContextExt ncRef = NamingContextExtHelper
							.narrow(objRef);

					// bind the Object Reference in Naming
					String name = "EchoComplexTypeDefAny";
					NameComponent[] path = ncRef.to_name(name);
					ncRef.rebind(path, href);
					log.info("EchoImpl - echoref rebindato: " + ncRef);

					log.info("EchoServer ready and waiting ...");

					// wait for invocations from clients
					orb.run();
				} catch (Exception e) {
					log.severe("ERROR: " + e);
					e.printStackTrace(System.out);
				}
				log.info("EchoServer Exiting ...");
			}
		});
		servantThread.setDaemon(daemon);
		servantThread.start();
	}

	// ==========================================
	// The operations in the IDL
	// ==========================================

	public static Object transformFromAny(Any paramAny)
			throws SecurityException, NoSuchFieldException,
			IllegalArgumentException, IllegalAccessException,
			ClassNotFoundException, InvocationTargetException,
			InstantiationException, NoSuchMethodException {
		Object localObject = null;

		TCKind localTCKind = paramAny.type().kind();
		System.out.println("type code: " + localTCKind.value() + "  "
				+ localTCKind.toString());

		String typeId = null;
		try {
			typeId = paramAny.type().id();
		} catch (BadKind e) {
			e.printStackTrace();
		}
		
		String mystring ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/mystring:1.0";
		String myint ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myint:1.0";
		String mystringarr ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/mystringArr:1.0";
		String mystruct ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myEchoStruct:1.0";
		String myseq ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/MySequence:1.0";
		String mystrucseq = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoStructSeq:1.0";
		String mymatrix = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/MatrixString:1.0";
		String myintseqofseq = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myintSeqOfSeq:1.0";
		
		if (localTCKind.equals(TCKind.tk_string))
			localObject = paramAny.extract_string();
		else if (localTCKind.equals(TCKind.tk_long))
			localObject = new Integer(paramAny.extract_long());
		else if (localTCKind.equals(TCKind.tk_double))
			localObject = new Double(paramAny.extract_double());
		else if (localTCKind.equals(TCKind.tk_boolean))
			localObject = new Boolean(paramAny.extract_boolean());
		else if (localTCKind.equals(TCKind.tk_float))
			localObject = new Float(paramAny.extract_float());
		else if (localTCKind.equals(TCKind.tk_char))
			localObject = new Character(paramAny.extract_char());
		else if (localTCKind.equals(TCKind.tk_fixed)) {
			localObject = paramAny.extract_fixed();
			System.out.println(">>>> type: fixed, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_longdouble)) // ??
		{
			localObject = new Double(paramAny.extract_double());
			System.out.println(">>>> type: longDouble, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_longlong)) {
			localObject = new Long(paramAny.extract_longlong());
			System.out.println(">>>> type: longlong, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_null)) {
			localObject = null;
			System.out.println(">>>> type: null, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_octet)) {
			localObject = new Byte(paramAny.extract_octet());
			System.out.println(">>>> type: octet, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_short)) {
			localObject = new Short(paramAny.extract_ushort());
			System.out.println(">>>> type: ushort, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_wchar)) {
			localObject = new Character(paramAny.extract_wchar());
			System.out.println(">>>> type: wchar, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_wstring)) {
			localObject = new String(paramAny.extract_wstring());
			System.out.println(">>>> type: wstring, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_any)) {
			localObject = transformFromAny(paramAny.extract_any());
			System.out.println(">>>> type: any, value: " + localObject);
		}

		else if ((localTCKind.equals(TCKind.tk_array))
				|| (localTCKind.equals(TCKind.tk_alias))) {
			// localObject = transformFromAny(paramAny, orb, serviceDescriptor);
			System.out.println(">>>> type: array or alias, value: " + paramAny);
    		
    		if (mystring.equals(typeId)) {
    			String myString = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mystringHelper.extract(paramAny);
    			localObject = myString;	                                                                
    		}else if (myint.equals(typeId)) {
    			int myintValue = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myintHelper.extract(paramAny);
				return myintValue;	                                                                
    		}else if (mystringarr.equals(typeId)) {
    			String[] mystringArrValue = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mystringArrHelper.extract(paramAny);
    			String result = "";
    			for(int i=0; i < mystringArrValue.length; i++){
    				result = result +" "+ mystringArrValue[i];
    			}  
				return result;	                                                                
    		}else if (mystruct.equals(typeId)) {
    			EchoStruct mystructValue = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myEchoStructHelper.extract(paramAny);
    			String result = "";
    			result = mystructValue.fieldString;  			
				return result;	                                                                
    		}else if (mystrucseq.equals(typeId)) {
    			
    			EchoStruct[] mystructValues = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoStructSeqHelper.extract(paramAny);
    			String result = "";
    			for(int i = 0 ; i < mystructValues.length; i++){
    				result = result + " "+ mystructValues[i].fieldString;		
	    			
    			}
    			return result;	                                                                
    		}else if (mymatrix.equals(typeId)) {
    			
    			String[][] mymatrixValues = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MatrixStringHelper.extract(paramAny);
    			String result = "";
    			if(mymatrixValues != null && mymatrixValues.length > 0 ){
	    			for(int i = 0 ; i < mymatrixValues.length; i++){
	    				for(int j = 0 ; j < mymatrixValues[0].length; j++){
	    					result = result + " "+ mymatrixValues[i][j];
	    				}
	    			}
	    			return result;	 
	    		}
				return result;	                                                                
    		}else if (myintseqofseq.equals(typeId)) {
    			
    			int[][] mymatrixValues = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myintSeqOfSeqHelper.extract(paramAny);
    			String result = "";
    			if(mymatrixValues != null && mymatrixValues.length > 0 ){
	    			for(int i = 0 ; i < mymatrixValues.length; i++){
	    				for(int j = 0 ; j < mymatrixValues[0].length; j++){
	    					result = result + " "+ mymatrixValues[i][j];
	    				}
	    			}
	    			return result;	 
	    		}
				return result;	                                                                
    		}
    		
		} else if (localTCKind.equals(TCKind.tk_sequence))
			try {
				System.out.println(">>>> type: sequence, value: " + paramAny);
				DynAny[] arrayOfAny = DynAnySeqHelper.extract(paramAny);

				Object[] arrayOfObject = new Object[arrayOfAny.length];

				for (int j = 0; j < arrayOfAny.length; ++j) {
					arrayOfObject[j] = transformFromAny(arrayOfAny[j].get_any());
				}

				localObject = arrayOfObject;
			} catch (TypeMismatch e) {
				// TODO
				System.out.println("Error transforming Any as a tk_sequence"
						+ e.getMessage());
			} catch (InvalidValue e) {
				// TODO
				System.out.println("Error transforming Any as a tk_sequence"
						+ e.getMessage());
			}

		else if (localTCKind.equals(TCKind.tk_objref)) {
			// TODO how to treat org.omg.CORBA.Object
			localObject = paramAny.extract_Object();
			System.out.println(">>>> type: objref, value: " + paramAny);

		} else if (localTCKind.equals(TCKind.tk_union)
				|| localTCKind.equals(TCKind.tk_struct)
				|| localTCKind.equals(TCKind.tk_value)
				|| localTCKind.equals(TCKind.tk_value_box)
				|| localTCKind.equals(TCKind.tk_enum)
				|| localTCKind.equals(TCKind.tk_except)
				|| localTCKind.equals(TCKind.tk_native)
				|| localTCKind.equals(TCKind.tk_abstract_interface)) {
			localObject = extractCorbaTypeFromAny(paramAny,
					extractTypeFromTypeCode(paramAny.type()), Thread
							.currentThread().getContextClassLoader());

			if (myseq.equals(typeId)) {
    			MySequence myseqValue = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MySequenceHelper.extract(paramAny);
    			
    	        int[] datamyseqValue = myseqValue.data;
    	        String result = "";
    			for(int i=0; i < datamyseqValue.length; i++){
    				result = result + " " + datamyseqValue[i];
    			}    			
				return result;	                                                                
    		}
			System.out.println(">>>> type: union or struct, value: "
					+ localObject);
		}
		return localObject;
	}

	public static String extractTypeFromTypeCode(TypeCode tc) {
		try {
			String tempType = tc.id();
			int idxStart = tempType.indexOf(':');
			int idxEnd = tempType.lastIndexOf('/');
			if (idxEnd < 0) {
				idxEnd = tempType.lastIndexOf('\\');
			}
			tempType = tempType.substring(idxStart + 1, idxEnd);
			return tempType.replace('\\', '.').replace('/', '.');
		} catch (BadKind e) {
			return null;
		}
	}

	public static void encloseCorbaTypeInAny(Any paramAny, Object origObj,
			Class typeClass, ClassLoader classLoader) {
		Class typeHelperClass = null;
		try {
			typeHelperClass = classLoader.loadClass(typeClass.getName()
					+ "Helper");

			if (typeHelperClass != null) {
				Method method = typeHelperClass.getMethod("insert", Any.class,
						classLoader.loadClass(origObj.getClass().getName()));
				try {
					method.invoke(null, paramAny, origObj);
				} catch (IllegalArgumentException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvocationTargetException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			}
		} catch (ClassNotFoundException e) {

		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static Object extractCorbaTypeFromAny(Any paramAny,
			String typeClass, ClassLoader classLoader) {
		Class typeHelperClass = null;
		try {
			typeHelperClass = classLoader.loadClass(typeClass + "Helper");

			if (typeHelperClass != null) {
				Method method = typeHelperClass.getMethod("extract", Any.class);
				try {
					Object obj = method.invoke(null, paramAny);

					return obj;
				} catch (IllegalArgumentException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvocationTargetException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			}
		} catch (ClassNotFoundException e) {

		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public static Object transformFromAnyHolder(Any paramAny)
			throws SecurityException, NoSuchFieldException,
			IllegalArgumentException, IllegalAccessException,
			ClassNotFoundException, InvocationTargetException,
			InstantiationException, NoSuchMethodException {
		Object localObject = null;

		String mystring ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/mystring:1.0";
		String myint ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myint:1.0";
		String mystringarr ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/mystringArr:1.0";
		String mystruct ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myEchoStruct:1.0";
		String myseq ="IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/MySequence:1.0";
		String mystrucseq = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoStructSeq:1.0";
		String mymatrix = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/MatrixString:1.0";
		String myintseqofseq = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myintSeqOfSeq:1.0";
		
		String typeId = null;
		try {
			typeId = paramAny.type().id();
		} catch (BadKind e) {
			e.printStackTrace();
		}
		
		TCKind localTCKind = paramAny.type().kind();
		System.out.println("type code: " + localTCKind.value() + "  "
				+ localTCKind.toString());

		if (localTCKind.equals(TCKind.tk_string))
			paramAny.insert_string(paramAny.extract_string() + " changed");
		else if (localTCKind.equals(TCKind.tk_long))
			paramAny.insert_long(new Integer(paramAny.extract_long() + 1));
		else if (localTCKind.equals(TCKind.tk_double))
			paramAny.insert_double(new Double(paramAny.extract_double() + 1));
		else if (localTCKind.equals(TCKind.tk_boolean))
			paramAny.insert_boolean(new Boolean(!paramAny.extract_boolean()));
		else if (localTCKind.equals(TCKind.tk_float))
			paramAny.insert_float(new Float(paramAny.extract_float() + 1));
		else if (localTCKind.equals(TCKind.tk_char))
			paramAny.insert_char('z');
		else if (localTCKind.equals(TCKind.tk_fixed)) {
			paramAny.insert_fixed(paramAny.extract_fixed());

			System.out.println(">>>> type: fixed, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_longdouble)) // ??
		{
			paramAny.insert_double(new Double(paramAny.extract_double() + 1));
			System.out.println(">>>> type: longDouble, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_longlong)) {
			paramAny.insert_longlong(new Long(paramAny.extract_longlong() + 1));
			System.out.println(">>>> type: longlong, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_null)) {
			localObject = null;
			System.out.println(">>>> type: null, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_octet)) {
			paramAny.insert_octet(new Byte(
					(byte) (paramAny.extract_octet() & 0x000000FF)));
			System.out.println(">>>> type: octet, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_short)) {
			paramAny.insert_ushort(new Short(
					(short) (paramAny.extract_ushort() + 1)));
			System.out.println(">>>> type: ushort, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_wchar)) {
			localObject = new Character(paramAny.extract_wchar());
			System.out.println(">>>> type: wchar, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_wstring)) {
			localObject = new String(paramAny.extract_wstring());
			System.out.println(">>>> type: wstring, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_any)) {
			localObject = transformFromAny(paramAny.extract_any());
			System.out.println(">>>> type: any, value: " + localObject);
		}

		else if ((localTCKind.equals(TCKind.tk_array))
				|| (localTCKind.equals(TCKind.tk_alias))) {
			// localObject = transformFromAny(paramAny, orb, serviceDescriptor);
			System.out.println(">>>> type: array or alias, value: " + paramAny+" - type : "+paramAny.type());

			System.out.println(">>>> class :" +paramAny.getClass());
    		
    		if (mystring.equals(typeId)) {
    			String myString = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mystringHelper.extract(paramAny);
    			String newMyString = myString + "_mod";    			
    			it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mystringHelper.insert(paramAny, newMyString);
				return myString;	                                                                
    		}else if (myint.equals(typeId)) {
    			int myintValue = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myintHelper.extract(paramAny);
    			int newMyintValue = myintValue +4;    			
    			it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myintHelper.insert(paramAny, newMyintValue);
				return newMyintValue;	                                                                
    		}else if (mystringarr.equals(typeId)) {
    			String[] mystringArrValue = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mystringArrHelper.extract(paramAny);
    			String[] newmystringArrValue = new String[mystringArrValue.length];
    			for(int i=0; i < mystringArrValue.length; i++){
    				newmystringArrValue[i] = mystringArrValue[i]+"_mod";
    			}    			
    			it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mystringArrHelper.insert(paramAny, newmystringArrValue);
				return newmystringArrValue;	                                                                
    		}else if (mystruct.equals(typeId)) {
    			EchoStruct mystructValue = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myEchoStructHelper.extract(paramAny);
    			EchoStruct newEs = new EchoStruct(mystructValue.fieldBoolean, 
    											mystructValue.fieldChar, 
    											mystructValue.fieldChar, 
    											mystructValue.fieldOctet, 
    											mystructValue.fieldString+"_mod", 
    											mystructValue.fieldWString, 
    											mystructValue.fieldShort, 
    											mystructValue.fieldUnsignedShort, 
    											mystructValue.fieldLong, 
    											mystructValue.fieldUnsignedLong, 
    											mystructValue.fieldLongLong, 
    											mystructValue.fieldUnsignedLongLong, 
    											mystructValue.fieldFloat, 
    											mystructValue.fieldDouble, 
    											mystructValue.fieldAny);  			
    			it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myEchoStructHelper.insert(paramAny, newEs);
				return newEs;	                                                                
    		}else if (mystrucseq.equals(typeId)) {
    			
    			EchoStruct[] mystructValues = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoStructSeqHelper.extract(paramAny);
    			EchoStruct[] newEss = new EchoStruct[mystructValues.length];
    			for(int i = 0 ; i < mystructValues.length; i++){
	    			newEss[i] = new EchoStruct(mystructValues[i].fieldBoolean, 
	    											mystructValues[i].fieldChar, 
	    											mystructValues[i].fieldChar, 
	    											mystructValues[i].fieldOctet, 
	    											mystructValues[i].fieldString+"_mod", 
	    											mystructValues[i].fieldWString, 
	    											mystructValues[i].fieldShort, 
	    											mystructValues[i].fieldUnsignedShort, 
	    											mystructValues[i].fieldLong, 
	    											mystructValues[i].fieldUnsignedLong, 
	    											mystructValues[i].fieldLongLong, 
	    											mystructValues[i].fieldUnsignedLongLong, 
	    											mystructValues[i].fieldFloat, 
	    											mystructValues[i].fieldDouble, 
	    											mystructValues[i].fieldAny);  			
	    			
    			}
    			it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoStructSeqHelper.insert(paramAny, newEss);
				return newEss;	                                                                
    		}else if (mymatrix.equals(typeId)) {
    			
    			String[][] mymatrixValues = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MatrixStringHelper.extract(paramAny);
    			if(mymatrixValues != null && mymatrixValues.length > 0 ){
	    			String[][] newmymatrixValues = new String[mymatrixValues.length][mymatrixValues[0].length];
	    			for(int i = 0 ; i < mymatrixValues.length; i++){
	    				for(int j = 0 ; j < mymatrixValues[0].length; j++){
	    					newmymatrixValues[i][j] = mymatrixValues[i][j] + "_mod";
	    				}
	    			}
	    			
	    			it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MatrixStringHelper.insert(paramAny, newmymatrixValues);
	    			return newmymatrixValues;
	    		}
				return mymatrixValues;	                                                                
    		}else if (myintseqofseq.equals(typeId)) {
    			
    			int[][] mymatrixValues = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myintSeqOfSeqHelper.extract(paramAny);
    			if(mymatrixValues != null && mymatrixValues.length > 0 ){
	    			int[][] newmymatrixValues = new int[mymatrixValues.length][mymatrixValues[0].length];
	    			for(int i = 0 ; i < mymatrixValues.length; i++){
	    				for(int j = 0 ; j < mymatrixValues[0].length; j++){
	    					newmymatrixValues[i][j] = mymatrixValues[i][j] + 1;
	    				}
	    			}
	    			
	    			it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myintSeqOfSeqHelper.insert(paramAny, newmymatrixValues);
	    			return newmymatrixValues;
	    		}
				return mymatrixValues;	                                                                
    		}
    		
    		
    		
    		
			
			
		} else if (localTCKind.equals(TCKind.tk_sequence))
			try {
				System.out.println(">>>> type: sequence, value: " + paramAny);
				DynAny[] arrayOfAny = DynAnySeqHelper.extract(paramAny);

				Object[] arrayOfObject = new Object[arrayOfAny.length];

				for (int j = 0; j < arrayOfAny.length; ++j) {
					arrayOfObject[j] = transformFromAny(arrayOfAny[j].get_any());
				}

				localObject = arrayOfObject;
			} catch (TypeMismatch e) {
				// TODO
				System.out.println("Error transforming Any as a tk_sequence"
						+ e.getMessage());
			} catch (InvalidValue e) {
				// TODO
				System.out.println("Error transforming Any as a tk_sequence"
						+ e.getMessage());
			}

		else if (localTCKind.equals(TCKind.tk_objref)) {
			// TODO how to treat org.omg.CORBA.Object
			localObject = paramAny.extract_Object();
			System.out.println(">>>> type: objref, value: " + paramAny);

		} else if (localTCKind.equals(TCKind.tk_union)
				|| localTCKind.equals(TCKind.tk_struct)
				|| localTCKind.equals(TCKind.tk_value)
				|| localTCKind.equals(TCKind.tk_value_box)
				|| localTCKind.equals(TCKind.tk_enum)
				|| localTCKind.equals(TCKind.tk_except)
				|| localTCKind.equals(TCKind.tk_native)
				|| localTCKind.equals(TCKind.tk_abstract_interface)) {
			System.out.println(">>>> type: union or struct, value...: "
					+ paramAny);
			localObject = extractCorbaTypeFromAny(paramAny,
					extractTypeFromTypeCode(paramAny.type()), Thread
							.currentThread().getContextClassLoader());
			System.out.println(">>>> object: " + localObject);
			
			if (myseq.equals(typeId)) {
    			MySequence myseqValue = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MySequenceHelper.extract(paramAny);
    			
    	        int[] datamyseqValue = myseqValue.data;
    	        
    			for(int i=0; i < datamyseqValue.length; i++){
    				datamyseqValue[i] = datamyseqValue[i] + 1;
    			}    			
    			myseqValue.data = datamyseqValue;
    			it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MySequenceHelper.insert(paramAny, myseqValue);
				return myseqValue;	                                                                
    		}
			
		}
		return localObject;
	}

	public org.omg.CORBA.Any echo(org.omg.CORBA.Any msg) {
		System.out.println("Received message: " + msg);
		Object myObject = null;
		try {
			myObject = transformFromAny(msg);

		} catch (SecurityException ex) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.SEVERE, null, ex);
		} catch (NoSuchFieldException ex) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.SEVERE, null, ex);
		} catch (IllegalArgumentException ex) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.SEVERE, null, ex);
		} catch (IllegalAccessException ex) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.SEVERE, null, ex);
		} catch (ClassNotFoundException ex) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.SEVERE, null, ex);
		} catch (InvocationTargetException ex) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.SEVERE, null, ex);
		} catch (InstantiationException ex) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.SEVERE, null, ex);
		} catch (NoSuchMethodException ex) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.SEVERE, null, ex);
		}

		if (myObject != null) {
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.INFO,
					"Oggetto di classe: " + myObject.getClass().getName());
			Logger.getLogger(EchoComplexTypeDefAnyImpl.class.getName()).log(
					Level.INFO, "Valore: " + myObject);
			;
		}
		return msg;
	}

	public String echoinout(org.omg.CORBA.AnyHolder msg) {
		try {
			transformFromAnyHolder(msg.value);
			Object obj = transformFromAny(msg.value);
			if(obj != null)
				return obj.toString();
		} catch (SecurityException ex) {
			System.out.println("Error " + ex.getMessage());
		} catch (NoSuchFieldException ex) {
			System.out.println("Error " + ex.getMessage());
		} catch (IllegalArgumentException ex) {
			System.out.println("Error " + ex.getMessage());
		} catch (IllegalAccessException ex) {
			System.out.println("Error " + ex.getMessage());
		} catch (ClassNotFoundException ex) {
			System.out.println("Error " + ex.getMessage());
		} catch (InvocationTargetException ex) {
			System.out.println("Error " + ex.getMessage());
		} catch (InstantiationException ex) {
			System.out.println("Error " + ex.getMessage());
		} catch (NoSuchMethodException ex) {
			System.out.println("Error " + ex.getMessage());
		}
		return "error";
	}

}