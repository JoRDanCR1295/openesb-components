/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovideranytypes;

import it.imolinfo.jbi4corba.test.testprovideranytypes.*;
import java.io.InputStream;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.net.URLClassLoader;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.omg.CORBA.Any;
import org.omg.CORBA.AnySeqHelper;
import org.omg.CORBA.BooleanSeqHelper;
import org.omg.CORBA.FloatSeqHelper;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StringSeqHelper;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynAnySeqHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

/**
 * This class is the corba servant used to manage the 'AnyTypes.idl'
 */
public class EchoAnyTypesImpl extends EchoAnyTypesPOA {

  /**
   * logger.
   */
  private static Logger log = Logger.getLogger(EchoAnyTypesImpl.class.getName());
  //LogFactory.getLog(EchoImpl.class);
  
  static ORB orb = null;
  /**
   * main first argument is the port (host is supposed to be localhost) second argument is
   * daemon=true/false optional, default false.
   * If daemon is true the servan starts as daemon, useful for integration tests
   */
  public static void main(String[] args) throws Exception {
    String propertyFile = args[0];

    boolean daemon = args.length > 1 ? "daemon=true".equals(args[1]) : false;
    startCorbaServant(daemon, propertyFile);

  }

  private static void startOrbd(final String port) {
    Thread orbdThread = new Thread(new Runnable() {

      public void run() {
        log.info("starting orbd on port: " + port);
        com.sun.corba.se.impl.activation.ORBD.main(new String[]{"-ORBInitialPort", port, "-ORBInitialHost", "localhost"});
      }
    });
    orbdThread.setDaemon(true);
    orbdThread.start();
    log.info("orbd launched");
  }

  private static void startCorbaServant(final boolean daemon, final String orbPropertyFile) {
    Thread servantThread = new Thread(new Runnable() {

      public void run() {
        try {
          // create and initialize the ORB                    
          log.info("loading orb.properties: " + orbPropertyFile);

          InputStream is = this.getClass().getResourceAsStream("/" + orbPropertyFile);
          log.info("input stream: " + is);
          Properties props = new Properties();
          props.load(is);

          log.info("launching orb with properties: " + props);

          orb = ORB.init((String[]) null, props);

          // get reference to rootpoa & activate the POAManager
          POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
          rootpoa.the_POAManager().activate();

          // create servant and register it with the ORB
          EchoAnyTypesImpl helloImpl = new EchoAnyTypesImpl();
          log.info("EchoImpl ..." + helloImpl);

          // get object reference from the servant
          org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
          EchoAnyTypes href = EchoAnyTypesHelper.narrow(ref);

          if (daemon) {
            startOrbd(props.getProperty("orbd.port"));
            Thread.currentThread().sleep(2000);
          }

          // get the root naming context
                    // NameService invokes the name service
          org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");

          // Use NamingContextExt which is part of the Interoperable
                    // Naming Service (INS) specification.
          NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

          // bind the Object Reference in Naming
          String name = "EchoAnyTypes";
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
    //                  The operations in the IDL
    // ==========================================

  public static Object transformFromAny(Any paramAny)
			throws SecurityException, NoSuchFieldException,
			IllegalArgumentException, IllegalAccessException,
			ClassNotFoundException, InvocationTargetException,
			InstantiationException, NoSuchMethodException {
		Object localObject = null;

		TCKind localTCKind = paramAny.type().kind();
System.out.println("type code: " + localTCKind.value() + "  " + localTCKind.toString());

		if (localTCKind.equals(TCKind.tk_string) )
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
			//Object[] values = (Object[])paramAny.extract_Value();
			//System.out.println(">>>> type value:" + paramAny.extract_Value());
		} else if (localTCKind.equals(TCKind.tk_sequence))
			try {
				System.out.println(">>>> type: sequence, value: " + paramAny);
				DynAny[] arrayOfAny = DynAnySeqHelper.extract(paramAny);

				Object[] arrayOfObject = new Object[arrayOfAny.length];

				for (int j = 0; j < arrayOfAny.length; ++j) {
					arrayOfObject[j] = transformFromAny(
							arrayOfAny[j].get_any());
				}

				localObject = arrayOfObject;
			} catch (TypeMismatch e) {
				// TODO
				System.out.println("Error transforming Any as a tk_sequence" + e.getMessage());
			} catch (InvalidValue e) {
				// TODO
				System.out.println("Error transforming Any as a tk_sequence" + e.getMessage());
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
                       localObject = extractCorbaTypeFromAny(paramAny,extractTypeFromTypeCode(paramAny.type()),Thread.currentThread().getContextClassLoader())  ;
                      
                        System.out.println(">>>> type: union or struct, value: " + localObject);
                }
return localObject;
		}

  
  public static String extractTypeFromTypeCode(TypeCode tc)
	{
		try {
			String tempType = tc.id();
			int idxStart = tempType.indexOf(':');
			int idxEnd = tempType.lastIndexOf('/');
			if (idxEnd < 0)
			{
				idxEnd = tempType.lastIndexOf('\\');
			}
			tempType = tempType.substring(idxStart + 1, idxEnd);
			return tempType.replace('\\', '.').replace('/', '.');
		} catch (BadKind e) {
			return null;
		}
	}
  
  public static void encloseCorbaTypeInAny(Any paramAny,
			Object origObj, Class typeClass, ClassLoader classLoader)
			 {
		Class typeHelperClass = null;
		try {
			typeHelperClass = classLoader.loadClass( typeClass.getName()
					+ "Helper");
		

		if (typeHelperClass != null) {
			Method method = typeHelperClass.getMethod("insert", Any.class, classLoader.loadClass(origObj.getClass().getName()));
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
			 String typeClass, ClassLoader classLoader)
			 {
		Class typeHelperClass = null;
		try {
			typeHelperClass = classLoader.loadClass( typeClass
					+ "Helper");
		

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

		TCKind localTCKind = paramAny.type().kind();
System.out.println("type code: " + localTCKind.value() + "  " + localTCKind.toString());

		if (localTCKind.equals(TCKind.tk_string) )
			paramAny.insert_string(paramAny.extract_string() + " changed");
		else if (localTCKind.equals(TCKind.tk_long))
			paramAny.insert_long( new Integer(paramAny.extract_long() + 1));
		else if (localTCKind.equals(TCKind.tk_double))
			paramAny.insert_double(new Double(paramAny.extract_double() + 1));
		else if (localTCKind.equals(TCKind.tk_boolean))
			paramAny.insert_boolean( new Boolean(!paramAny.extract_boolean()));
		else if (localTCKind.equals(TCKind.tk_float))
			paramAny.insert_float(new Float(paramAny.extract_float() + 1));
		else if (localTCKind.equals(TCKind.tk_char))
			paramAny.insert_char('z');
		 else if (localTCKind.equals(TCKind.tk_fixed)) {
			paramAny.insert_fixed( paramAny.extract_fixed());
                    
			System.out.println(">>>> type: fixed, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_longdouble)) // ??
		{
			paramAny.insert_double( new Double(paramAny.extract_double() + 1));
			System.out.println(">>>> type: longDouble, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_longlong)) {
			paramAny.insert_longlong( new Long(paramAny.extract_longlong() + 1));
			System.out.println(">>>> type: longlong, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_null)) {
			localObject = null;
			System.out.println(">>>> type: null, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_octet)) {
			paramAny.insert_octet( new Byte((byte)(paramAny.extract_octet() & 0x000000FF)));
			System.out.println(">>>> type: octet, value: " + localObject);
		} else if (localTCKind.equals(TCKind.tk_short)) {
			paramAny.insert_ushort( new Short((short)(paramAny.extract_ushort() + 1)));
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
			//Object[] values = (Object[])paramAny.extract_Value();
		//	System.out.println(">>>> type value:" + paramAny.extract_Value());
		} else if (localTCKind.equals(TCKind.tk_sequence))
			try {
				System.out.println(">>>> type: sequence, value: " + paramAny);
				DynAny[] arrayOfAny = DynAnySeqHelper.extract(paramAny);

				Object[] arrayOfObject = new Object[arrayOfAny.length];

				for (int j = 0; j < arrayOfAny.length; ++j) {
					arrayOfObject[j] = transformFromAny(
							arrayOfAny[j].get_any());
				}

				localObject = arrayOfObject;
			} catch (TypeMismatch e) {
				// TODO
				System.out.println("Error transforming Any as a tk_sequence" + e.getMessage());
			} catch (InvalidValue e) {
				// TODO
				System.out.println("Error transforming Any as a tk_sequence" + e.getMessage());
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
			System.out.println(">>>> type: union or struct, value...: " + paramAny);
                        localObject =extractCorbaTypeFromAny(paramAny,extractTypeFromTypeCode(paramAny.type()), Thread.currentThread().getContextClassLoader());
			 System.out.println(">>>> object: " + localObject);
                }
return localObject;
		}
  
  public String echo1 (org.omg.CORBA.AnyHolder msg)
  {
        try {
             transformFromAnyHolder(msg.value);
            Object obj = transformFromAny(msg.value);
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
  
  
  
  public org.omg.CORBA.Any echo2 (org.omg.CORBA.Any msg) throws it.imolinfo.jbi4corba.test.testprovideranytypes.EchoAnyException
  {
       try {
            Object obj = transformFromAny(msg);
            Any result = orb.create_any();
            
            
            if (msg.type().kind() == TCKind.tk_union)
            {
                System.out.println("Is Union:");
                encloseCorbaTypeInAny(result, obj, obj.getClass(),Thread.currentThread().getContextClassLoader());
                System.out.print(obj);
            }
            else
            if (obj.toString().equals("anyString"))
            {
                result.insert_string("Any with String return type");
                
            }else if (obj.toString().equals("anyAny"))
            {
                Any newAny = orb.create_any();
                newAny.insert_string(obj.toString() + " new");
                result.insert_any(newAny);
              
            }else if (obj.toString().equals("anyUnion"))
            {
                TempUnion union = new TempUnion();
                union.alfanumeric("Union embeded in Any");
                encloseCorbaTypeInAny(result, union, TempUnion.class, Thread.currentThread().getContextClassLoader());
               
            }else if (obj.toString().equals("exceptionString"))
            {
                result.insert_wstring("This is an Any Exception");
                EchoAnyException exc = new EchoAnyException(result);
                throw exc;
            }
            else if (obj.toString().equals("exceptionStruct"))
            {
                ComplexStruct1 cs1 = new ComplexStruct1();
                cs1.fieldBoolean = true;
                cs1.fieldChar = 'c';
                Any newAny = orb.create_any();
                newAny.insert_string(obj.toString() + " new");
                cs1.fieldAny = new Any[1];
                cs1.fieldAny[0] = newAny;
                
                encloseCorbaTypeInAny(result, cs1, ComplexStruct1.class, Thread.currentThread().getContextClassLoader());
                EchoAnyException exc = new EchoAnyException(result);
                throw exc;
            }
            else if (obj.toString().equals("exceptionUnion"))
            {
                TempUnion union = new TempUnion();
                union.alfanumeric("Exception with Union embeded in Any");
                encloseCorbaTypeInAny(result, union, TempUnion.class, Thread.currentThread().getContextClassLoader());
                EchoAnyException exc = new EchoAnyException(result);
                throw exc;
            }
            else if (obj.toString().equals("anyStruct"))
            {
                ComplexStruct1 cs1 = new ComplexStruct1();
                cs1.fieldAny = (Any[])Array.newInstance(Any.class, 14);
                for (int i = 0; i < 14;i++)
                {
                    cs1.fieldAny[i] = orb.create_any();
                    if (i == 0)
                        cs1.fieldAny[i].insert_string(obj.toString());
                    if (i == 1)
                        cs1.fieldAny[i].insert_long(20);
                    if (i == 2)
                        cs1.fieldAny[i].insert_longlong(25L);
                    if (i == 3)
                        cs1.fieldAny[i].insert_double(30D);
                    if (i == 4)
                        cs1.fieldAny[i].insert_boolean(true);
                    if (i == 5)
                        cs1.fieldAny[i].insert_float(20.4f);
                    if (i == 6)
                        cs1.fieldAny[i].insert_char('q');
                    if (i == 7)
                        cs1.fieldAny[i].insert_fixed(new BigDecimal("123123123123"));
                    if (i == 8)
                        cs1.fieldAny[i].insert_longlong(40L);
                    if (i == 9)
                        cs1.fieldAny[i].insert_octet((byte)0x000000FF);
                    if (i == 10)
                        cs1.fieldAny[i].insert_short((short)10);
                    if (i == 11)
                        cs1.fieldAny[i].insert_wchar('w');
                    if (i == 12)
                        cs1.fieldAny[i].insert_wstring("wstring");
                    if (i == 13)
                    {
                        Any any = orb.create_any();
                        any.insert_double(50D);
                        cs1.fieldAny[i].insert_any(any);
                    }
                    if (i == 14)
                    {
                        Any any = orb.create_any();
                        
                        cs1.fieldAny[i].insert_any(any);
                    }
                
                }
                cs1.fieldBoolean = true;
                cs1.fieldChar = 'c';
                encloseCorbaTypeInAny(result, cs1, ComplexStruct1.class, Thread.currentThread().getContextClassLoader());
                //result.insert_Value(cs1);
            
            }
            else if (obj.toString().equals("anyArrayOfAny"))
            {
                Any[][] arrOfAny = new Any[2][3];
                        for (int j = 0;j < 2;j++)
                        {
                            for (int k = 0;k < 3;k++)
                        {
                            arrOfAny[j][k] = orb.create_any();
                            arrOfAny[j][k].insert_string("any[" + j + "][" + k + "]");
                        }
                        }
                        ArrayOfAnyHelper.insert(result, arrOfAny);
            }
            else if(obj.toString().equals("anySeqOfString"))
            {
                String[] stringArr = new String[5];
                for (int i = 0;i < 5;i++)
                {
                    stringArr[i] = "string[" + i + "]";
                }
                StringSeqHelper.insert(result, stringArr);
            }
            else if(obj.toString().equals("anySeqOfBoolean"))
            {
                boolean[] boolArr = new boolean[5];
                for (int i = 0;i < 5;i++)
                {
                    boolArr[i] = true;
                }
                BooleanSeqHelper.insert(result, boolArr);
            }
            else if(obj.toString().equals("anySeqOfFloat"))
            {
                float[] floatArr = new float[5];
                for (int i = 0;i < 5;i++)
                {
                    floatArr[i] = 5.4f + i;
                }
                FloatSeqHelper.insert(result, floatArr);
            }
            else if(obj.toString().equals("anySeqOfAny"))
            {
                 System.out.println("Is Seq of Any!:");
                Any[] anyArr = new Any[5];
                for (int i = 0;i < 5;i++)
                {
                    anyArr[i] = orb.create_any();
                    anyArr[i].insert_string("index " + i );
                }
                AnySeqHelper.insert(result, anyArr);
            }
            
            
            return result;
            
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
	  return msg;
  }
  
  public it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1 echo3 (it.imolinfo.jbi4corba.test.testprovideranytypes.TempUnion msg)
  {
	  ComplexStruct1 cs1 = new ComplexStruct1();
	  Any any = msg.two_format();
        try {
            Object obj = transformFromAny(any);
            Any newAny = orb.create_any();
            newAny.insert_string(obj.toString() + " new");
            cs1.fieldAny = new Any[1];
            cs1.fieldAny[0] = newAny;
                    
        } catch (SecurityException ex) {
            Logger.getLogger(EchoAnyTypesImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoSuchFieldException ex) {
            Logger.getLogger(EchoAnyTypesImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(EchoAnyTypesImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            Logger.getLogger(EchoAnyTypesImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(EchoAnyTypesImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvocationTargetException ex) {
            Logger.getLogger(EchoAnyTypesImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            Logger.getLogger(EchoAnyTypesImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoSuchMethodException ex) {
            Logger.getLogger(EchoAnyTypesImpl.class.getName()).log(Level.SEVERE, null, ex);
        }
	  return cs1;
  }
  public org.omg.CORBA.Any[][] echo4 (it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1Holder msg)
  {
	  return null;
  }
 
  
}