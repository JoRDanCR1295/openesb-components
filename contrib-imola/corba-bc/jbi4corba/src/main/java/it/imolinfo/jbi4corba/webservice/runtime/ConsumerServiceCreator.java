 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.webservice.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ServiceActivationException;
import it.imolinfo.jbi4corba.exception.ServiceCreationException;
import it.imolinfo.jbi4corba.webservice.descriptor.ConsumerServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.ChildFirstClassLoader;
import it.imolinfo.jbi4corba.webservice.generator.ServerCorbaClassesHolder;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.Map;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CORBA.portable.ValueFactory;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.POAManagerPackage.AdapterInactive;

/**
 * This class is used to create the JBI service.
 */
public class ConsumerServiceCreator {

  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(ConsumerServiceCreator.class);

  /**
   * Default constructor.
   */
  public ConsumerServiceCreator() {
    // NOP
  }

//  /**
//   * This method is used to create the JBI service.
//   *
//   * @param   consumerServiceDescriptor   The consumer service descriptor
//   * @throws  ServiceCreationException    The service creation exception
//   */
//  public void createJbiService_old(
//    ConsumerServiceDescriptor consumerServiceDescriptor)
//    throws ServiceCreationException {
//
//		ClassLoader oldClassLoader = Thread.currentThread()
//				.getContextClassLoader();
//		try {
//
//			Thread.currentThread().setContextClassLoader(
//					consumerServiceDescriptor.getServerCorbaClassesHolder()
//							.getOriginalClassLoader());
//
//			// get the generated classes from the descriptor
//			ServerCorbaClassesHolder serverCorbaClassesHolder = consumerServiceDescriptor
//					.getServerCorbaClassesHolder();
//
//			// create and set up the invocation handler
//			ConsumerInvocationHandler consumerInvocationHandler = new ConsumerInvocationHandler(
//					consumerServiceDescriptor);
//			consumerServiceDescriptor
//					.setConsumerInvocationHandler(consumerInvocationHandler);
//
//			// create corbaImpl
//			Class corbaImplClass = serverCorbaClassesHolder.getCorbaImplClass();
//			Constructor corbaImplConstructor = getConstructorWithConsumerInvocationHandler(corbaImplClass);
//			Object corbaImpl = newObject(corbaImplConstructor,
//					new Object[] { consumerInvocationHandler });
//			LOG.debug("new corbaImpl=" + corbaImpl);
//
//			// creating orb
//			ORB orb = ORB.init((String[]) null, consumerServiceDescriptor
//					.getOrbProperties());
//			LOG.debug("ORB.init ... ok");
//			consumerServiceDescriptor.setOrb(orb);
//
//			// creating poatie
//			Constructor constructor = null;
//			Servant poaTie = null;
//			
//			if (serverCorbaClassesHolder.isGenerateClassesFromIDL()) {
//				
//				// registering value factory for value types
//				Map<String, java.lang.Object> map = consumerServiceDescriptor
//						.getServerCorbaClassesHolder()
//						.getValueTypeIdAndInstance();
//
//				if (map == null || map.size() == 0) {
//					LOG
//							.info("CRB000130_No_value_type_factory_to_register_using_the_ORB");
//				} else {
//					LOG.debug("ValueTypeFactoryMap.size =" + map.size());
//
//					// registering all valuetypes factories.
//					for (String id : map.keySet()) {
//						ValueFactory vf = (ValueFactory) map.get(id);
//
//						LOG.debug("Registering a ValueType Factory. id=" + id
//								+ "; instance=" + vf);
//
//						((org.omg.CORBA_2_3.ORB) orb).register_value_factory(
//								id, vf);
//					}
//				}
//
//				constructor = getConstructorFromPoaTie(serverCorbaClassesHolder
//						.getCorbaPOATie(), new Class[] {
//						serverCorbaClassesHolder.getCorbaOperations(),
//						POA.class });
//
//				LOG.debug("getConstructorFromPoaTie ... ok");
//
//				POA rootPoa;
//				try {
//					rootPoa = POAHelper.narrow(orb
//							.resolve_initial_references("RootPOA"));
//				} catch (InvalidName e) {
//					Object[] args = new Object[] { orb, e.getMessage() };
//
//					LOG.error(
//							"CRB000722_Unable_to_instantiate_resolve_RootPOA",
//							args, e);
//					throw new ServiceCreationException(
//							"CRB000722_Unable_to_instantiate_resolve_RootPOA",
//							args, e);
//				}
//
//				poaTie = newPoaTie(constructor, new Object[] { corbaImpl,
//						rootPoa });
//				LOG.debug("newPoaTie ... ok. Servant=" + poaTie);
//			} else {
//				constructor = getConstructorFromPoaTie(serverCorbaClassesHolder
//						.getCorbaPOATie(), new Class[] {});
//				LOG.debug("getDefaultConstructorFromPoaTie ... ok");
//
//				Object[] arguments = new Object[] {};
//				poaTie = newPoaTie(constructor, arguments);
//				LOG.debug("newPoaTie ... ok. Servant=" + poaTie);
//
//				// setOrb
//				setOrbOnPoaTie(poaTie, serverCorbaClassesHolder
//						.getCorbaPOATie(), orb);
//				LOG.debug("setup ORB on the Servant ... done");
//
//				// setTarget
//				setTargetOnPoaTie(poaTie, serverCorbaClassesHolder
//						.getCorbaPOATie(), corbaImpl); // Impl
//			}
//
//			LOG
//					.debug("Registering the Corba Servant Implementation on the Servant(TIE)"
//							+ "... done");
//
//			consumerServiceDescriptor.setPoaTie(poaTie);
//	} finally {
//		Thread.currentThread().setContextClassLoader(oldClassLoader);
//	}
//  }

  /**
	 * This method is used to create the JBI service.
	 * 
	 * @param consumerServiceDescriptor
	 *            The consumer service descriptor
	 * @throws ServiceCreationException
	 *             The service creation exception
	 */
	public void createJbiService(
			ConsumerServiceDescriptor consumerServiceDescriptor)
			throws ServiceCreationException {

		Constructor constructor = null;
		Servant poaTie = null;

		// get the generated classes from the descriptor
		ServerCorbaClassesHolder serverCorbaClassesHolder = consumerServiceDescriptor
				.getServerCorbaClassesHolder();
                
		// different behaviour if the classes were generated from IDL
		if (serverCorbaClassesHolder.isGenerateClassesFromIDL()) {

			ClassLoader oldClassLoader = Thread.currentThread()
					.getContextClassLoader();

			try {

				Thread.currentThread().setContextClassLoader(
						consumerServiceDescriptor.getServerCorbaClassesHolder()
								.getOriginalClassLoader());

				// creating orb
				ORB orb = ORB.init((String[]) null, consumerServiceDescriptor
						.getOrbProperties());
				LOG.debug("ORB.init ... ok");
				consumerServiceDescriptor.setOrb(orb);
				
				RuntimeInformation runtimeInfo = new RuntimeInformation(
						serverCorbaClassesHolder.getUrlClassLoader(),
						serverCorbaClassesHolder.getOriginalClassLoader()
						, serverCorbaClassesHolder
								.getAllCorbaTypes(), consumerServiceDescriptor
								.getEndpoint(), consumerServiceDescriptor
								.getOrb(), serverCorbaClassesHolder
								.getAllIDLTypes(), serverCorbaClassesHolder
								.getCorbaEnumMap(),serverCorbaClassesHolder.getIdToClassNameMap()
								, serverCorbaClassesHolder.getTypeDefs());
				
				consumerServiceDescriptor.setRuntimeInformation(runtimeInfo);

				// create and set up the invocation handler
				ConsumerInvocationHandler consumerInvocationHandler = new ConsumerInvocationHandler(
						consumerServiceDescriptor);
				consumerServiceDescriptor
						.setConsumerInvocationHandler(consumerInvocationHandler);

				// create corbaImpl
				Class corbaImplClass = serverCorbaClassesHolder
						.getCorbaImplClass();
				Constructor corbaImplConstructor = getConstructorWithConsumerInvocationHandler(corbaImplClass);
				Object corbaImpl = newObject(corbaImplConstructor,
						new Object[] { consumerInvocationHandler });
				LOG.debug("new corbaImpl=" + corbaImpl);

				// registering value factory for value types
				Map<String, java.lang.Object> map = consumerServiceDescriptor
						.getServerCorbaClassesHolder()
						.getValueTypeIdAndInstance();

				if (map == null || map.size() == 0) {
					LOG
							.debug("CRB000130_No_value_type_factory_to_register_using_the_ORB");
				} else {
					LOG.debug("ValueTypeFactoryMap.size =" + map.size());

					// registering all valuetypes factories.
					for (String id : map.keySet()) {
						ValueFactory vf = (ValueFactory) map.get(id);

						LOG.debug("Consumer Registering a ValueType Factory. id=" + id
								+ "; instance=" + vf);

						((org.omg.CORBA_2_3.ORB) orb).register_value_factory(
								id, vf);
					}
				}

				constructor = getConstructorFromPoaTie(serverCorbaClassesHolder
						.getCorbaPOATie(), new Class[] {
						serverCorbaClassesHolder.getCorbaOperations(),
						POA.class });

				LOG.debug("getConstructorFromPoaTie ... ok");

				POA rootPoa;
				try {
					rootPoa = POAHelper.narrow(orb
							.resolve_initial_references("RootPOA"));
				} catch (InvalidName e) {
					Object[] args = new Object[] { orb, e.getMessage() };

					LOG.error(
							"CRB000722_Unable_to_instantiate_resolve_RootPOA",
							args, e);
					throw new ServiceCreationException(
							"CRB000722_Unable_to_instantiate_resolve_RootPOA",
							args, e);
				}
                                
				poaTie = newPoaTie(constructor, new Object[] { corbaImpl,
						rootPoa });
				LOG.debug("newPoaTie ... ok. Servant=" + poaTie);

			} finally {
				Thread.currentThread().setContextClassLoader(oldClassLoader);
			}
		} else {

			// creating orb
			ORB orb = ORB.init((String[]) null, consumerServiceDescriptor
					.getOrbProperties());
			LOG.debug("ORB.init ... ok");
			consumerServiceDescriptor.setOrb(orb);

			// create and set up the invocation handler
			ConsumerInvocationHandler consumerInvocationHandler = new ConsumerInvocationHandler(
					consumerServiceDescriptor);
			consumerServiceDescriptor
					.setConsumerInvocationHandler(consumerInvocationHandler);

			// create corbaImpl
			Class corbaImplClass = serverCorbaClassesHolder.getCorbaImplClass();
			Constructor corbaImplConstructor = getConstructorWithConsumerInvocationHandler(corbaImplClass);
			Object corbaImpl = newObject(corbaImplConstructor,
					new Object[] { consumerInvocationHandler });
			LOG.debug("new corbaImpl=" + corbaImpl);
			constructor = getConstructorFromPoaTie(serverCorbaClassesHolder
					.getCorbaPOATie(), new Class[] {});
			LOG.debug("getDefaultConstructorFromPoaTie ... ok");

			Object[] arguments = new Object[] {};
			poaTie = newPoaTie(constructor, arguments);
			LOG.debug("newPoaTie ... ok. Servant=" + poaTie);

			// setOrb
			setOrbOnPoaTie(poaTie, serverCorbaClassesHolder.getCorbaPOATie(),
					orb);
			LOG.debug("setup ORB on the Servant ... done");

			// setTarget
			setTargetOnPoaTie(poaTie,
					serverCorbaClassesHolder.getCorbaPOATie(), corbaImpl); // Impl
		}

		LOG.debug("Registering the Corba Servant Implementation on the Servant(TIE)"
						+ "... done");

		consumerServiceDescriptor.setPoaTie(poaTie);
	}  
  /**
   * This method is used to set up the target class that the skeleton delegates
   * all the corba operations.
   *
   * @param   poaTie        The instance of the skeleton.
   * @param   poaTieClass   The class of the skeleton.
   * @param   poaDelegate   The delegate class.
   *
   * @throws  ServiceCreationException
   *              When the 'getMethod' or the 'invoke' via reflection cause an
   *              error.
   */
  protected void setTargetOnPoaTie(
    Servant poaTie,
    Class poaTieClass,
    Object poaDelegate) throws ServiceCreationException {

    // create

    Method setTargetMethod = null;
    try {

      setTargetMethod = poaTieClass.getMethod(
          "setTarget", new Class [] {java.rmi.Remote.class});

    } catch (SecurityException e) {
      Object[] args = new Object[] { "setTarget(java.rmi.Remote)",
                                     poaTieClass,
                                     e.getMessage() };

      LOG.error("CRB000741_CreatingMethodError", args, e);
      throw new ServiceCreationException(
              "CRB000741_CreatingMethodError", args, e);

    } catch (NoSuchMethodException e) {
      Object[] args = new Object[] { "setTarget(java.rmi.Remote)",
                                     poaTieClass,
                                     e.getMessage() };

      LOG.error("CRB000741_CreatingMethodError", args, e);
      throw new ServiceCreationException(
        "CRB000741_CreatingMethodError", args, e);
    }

    // invoke

    try {

      setTargetMethod.invoke(poaTie, new Object [] {poaDelegate});

    } catch (IllegalArgumentException e) {

      Object[] args = new Object[] { "setTarget(java.rmi.Remote)",
                                     poaTie,
                                     e.getMessage() };

      LOG.error("CRB000742_InvokingMethodError", args, e);
      throw new ServiceCreationException(
        "CRB000742_InvokingMethodError", args, e);

    } catch (IllegalAccessException e) {
      Object[] args = new Object[] { "setTarget(java.rmi.Remote)",
                                     poaTie,
                                     e.getMessage() };

      LOG.error("CRB000742_InvokingMethodError", args, e);
      throw new ServiceCreationException(
        "CRB000742_InvokingMethodError", args, e);
    } catch (InvocationTargetException e) {
      Object[] args = new Object[] { "setTarget(java.rmi.Remote)",
                                     poaTie,
                                     e.getMessage() };

      LOG.error("CRB000742_InvokingMethodError", args, e);
      throw new ServiceCreationException(
        "CRB000742_InvokingMethodError", args, e);
    }

  }

  /**
   * This method is used to set up the ORB in the skeleton.
   *
   * @param   poaTie        The instance of the skeleton.
   * @param   poaTieClass   The class of the skeleton.
   * @param   orb           The ORB.
   *
   * @throws  ServiceCreationException
   *              When the 'getMethod' or the 'invoke' via reflection cause an
   *              error.
   */
  protected void setOrbOnPoaTie(
    Servant poaTie,
    Class poaTieClass,
    ORB orb) throws ServiceCreationException {

    // create

    Method method = null;
    try {

      method
        = poaTieClass.getMethod("orb", new Class [] {org.omg.CORBA.ORB.class});

    } catch (SecurityException e) {
      Object[] args = new Object[] { "orb(org.omg.CORBA.ORB)",
                                     poaTieClass,
                                     e.getMessage() };

      LOG.error("CRB000741_CreatingMethodError", args, e);
      throw new ServiceCreationException(
              "CRB000741_CreatingMethodError", args, e);

    } catch (NoSuchMethodException e) {
      Object[] args = new Object[] { "orb(org.omg.CORBA.ORB)",
                                     poaTieClass,
                                     e.getMessage() };

      LOG.error("CRB000741_CreatingMethodError", args, e);
      throw new ServiceCreationException(
        "CRB000741_CreatingMethodError", args, e);
    }

    // invoke

    try {

      method.invoke(poaTie, new Object [] {orb});

    } catch (IllegalArgumentException e) {

      Object[] args = new Object[] { "orb(org.omg.CORBA.ORB)",
                                     poaTie,
                                     e.getMessage() };

      LOG.error("CRB000742_InvokingMethodError", args, e);
      throw new ServiceCreationException(
        "CRB000742_InvokingMethodError", args, e);

    } catch (IllegalAccessException e) {
      Object[] args = new Object[] { "orb(org.omg.CORBA.ORB)",
                                     poaTie,
                                     e.getMessage() };

      LOG.error("CRB000742_InvokingMethodError", args, e);
      throw new ServiceCreationException(
        "CRB000742_InvokingMethodError", args, e);
    } catch (InvocationTargetException e) {
      Object[] args = new Object[] { "orb(org.omg.CORBA.ORB)",
                                     poaTie,
                                     e.getMessage() };

      LOG.error("CRB000742_InvokingMethodError", args, e);
      throw new ServiceCreationException(
        "CRB000742_InvokingMethodError", args, e);
    }

  }

  /**
   * This method is used to create the default constructor of the Poa Tie Class.
   *
   * @param   poaTieClass   The skeleton.
   *
   * @return  The default constructor of the skeleton.
   *
   * @throws  ServiceCreationException
   *              When the 'getConstructor' via reflection causes an error.
   */
  protected Constructor getConstructorFromPoaTie(Class poaTieClass, Class[] classes)
    throws ServiceCreationException {

    Constructor constructor = null;

    try {

        constructor = poaTieClass.getConstructor(classes);

    } catch (SecurityException e) {
        Object[] args = new Object[] {
            poaTieClass, Arrays.toString(classes), e.getMessage() };

        LOG.error("CRB000723_Unable_to_create_constructor", args, e);
        throw new ServiceCreationException(
                "CRB000723_Unable_to_create_constructor", args, e);
    } catch (NoSuchMethodException e) {
        Object[] args = new Object[] {
            poaTieClass, Arrays.toString(classes), e.getMessage() };

        LOG.error("CRB000723_Unable_to_create_constructor", args, e);
        throw new ServiceCreationException(
                "CRB000723_Unable_to_create_constructor", args, e);
    }

    return constructor;
  }

  /**
   * This method is used to create the constructor where the input parameter
   * is a ConsumerInvocationHandler.
   *
   * @param   clazz   The class that contains the constructor.
   *
   * @return  The constructor.
   *
   * @throws  ServiceCreationException
   *              When the 'getConstructor' via reflection causes an error.
   */
  protected Constructor getConstructorWithConsumerInvocationHandler(
    Class clazz) throws ServiceCreationException {

    Constructor constructor = null;

    Class[] classes = new Class[] {ConsumerInvocationHandler.class};
    try {

        constructor = clazz.getConstructor(classes);

    } catch (SecurityException e) {
        Object[] args = new Object[] {
            clazz, Arrays.toString(classes), e.getMessage() };

        LOG.error("CRB000723_Unable_to_create_constructor", args, e);
        throw new ServiceCreationException(
                "CRB000723_Unable_to_create_constructor", args, e);
    } catch (NoSuchMethodException e) {
        Object[] args = new Object[] {
            clazz, Arrays.toString(classes), e.getMessage() };

        LOG.error("CRB000723_Unable_to_create_constructor", args, e);
        throw new ServiceCreationException(
                "CRB000723_Unable_to_create_constructor", args, e);
    }

    return constructor;
  }

  /**
   * This method creates the Servant using the constructor passed in input.
   *
   * @param   constructor     The default constructor of the skeleton.
   *
   * @return  The Servant created.
   *
   * @throws  ServiceCreationException
   *              When the 'new' via reflection causes an error.
   */
  protected Servant newPoaTie(Constructor constructor, Object[] arguments)
    throws ServiceCreationException {

    Servant poaTie = null;
    try {

        poaTie = (Servant) constructor.newInstance(arguments);

    } catch (IllegalArgumentException e) {
        Object[] args = new Object[] {
                constructor, "default constructor", e.getMessage() };

        LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
        throw new ServiceCreationException(
                "CRB000724_Unable_to_instatiate_object", args, e);
    } catch (InstantiationException e) {
        Object[] args = new Object[] {
                constructor, "default constructor", e.getMessage() };

        LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
        throw new ServiceCreationException(
                "CRB000724_Unable_to_instatiate_object", args, e);
    } catch (IllegalAccessException e) {
        Object[] args = new Object[] {
                constructor, "default constructor", e.getMessage() };

        LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
        throw new ServiceCreationException(
                "CRB000724_Unable_to_instatiate_object", args, e);
    } catch (InvocationTargetException e) {
        Throwable cause = e.getCause();
        Object[] args = new Object[] { constructor,
            "default constructor", cause.getMessage() };

        LOG.error("CRB000725_Exception_calling_constructor", args, cause);
        throw new ServiceCreationException(
                "CRB000725_Exception_calling_constructor", args, cause);
    }

    return poaTie;
  }

  /**
   * This method creates an Object.
   *
   * @param   constructor     A constructor of a class.
   * @param   parameter       The parameter for the constructor.
   *
   * @return  The Object created.
   *
   * @throws  ServiceCreationException
   *              When the 'new' via reflection causes an error.
   */
  protected Object newObject(Constructor constructor, Object [] parameter)
    throws ServiceCreationException {

    Object newObject = null;
    try {

      newObject = constructor.newInstance(parameter);

    } catch (IllegalArgumentException e) {
        Object[] args = new Object[] {
                constructor, "constructor", e.getMessage() };

        LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
        throw new ServiceCreationException(
                "CRB000724_Unable_to_instatiate_object", args, e);
    } catch (InstantiationException e) {
        Object[] args = new Object[] {
                constructor, "constructor", e.getMessage() };

        LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
        throw new ServiceCreationException(
                "CRB000724_Unable_to_instatiate_object", args, e);
    } catch (IllegalAccessException e) {
        Object[] args = new Object[] {
                constructor, "constructor", e.getMessage() };

        LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
        throw new ServiceCreationException(
                "CRB000724_Unable_to_instatiate_object", args, e);
    } catch (InvocationTargetException e) {
        Throwable cause = e.getCause();
        Object[] args = new Object[] { constructor,
            "default constructor", cause.getMessage() };

        LOG.error("CRB000725_Exception_calling_constructor", args, cause);
        throw new ServiceCreationException(
                "CRB000725_Exception_calling_constructor", args, cause);
    }

    return newObject;
  }

//------------------------------------------------------------------------------ PRE
    /**
     * @param consumerServiceDescriptor  The consumer service descriptor
     * @throws ServiceCreationException  The service creation exception
     */
    public void createJbiServicePRE(
      ConsumerServiceDescriptor consumerServiceDescriptor)
      throws ServiceCreationException {

      ServerCorbaClassesHolder serverCorbaClassesHolder
        = consumerServiceDescriptor.getServerCorbaClassesHolder();

      ConsumerInvocationHandler consumerInvocationHandler
        = new ConsumerInvocationHandler(consumerServiceDescriptor);

        consumerServiceDescriptor.setConsumerInvocationHandler(
                consumerInvocationHandler);
       
        LOG.debug("Trying to create the poaDelegate");
        // creating poa delegate
        Object poaDelegate = Proxy.newProxyInstance(
                serverCorbaClassesHolder.getCorbaPOATie().getClassLoader(),
                new Class[] { serverCorbaClassesHolder.getCorbaOperations() },
                consumerInvocationHandler);

        // creating orb
        ORB orb = ORB.init(
                (String[]) null, consumerServiceDescriptor.getOrbProperties());
        consumerServiceDescriptor.setOrb(orb);
        // creating poa tie
        POA rootPoa;
        try {
            rootPoa = POAHelper.narrow(
                    orb.resolve_initial_references("RootPOA"));
        } catch (InvalidName e) {
            Object[] args = new Object[] { orb, e.getMessage() };

            LOG.error("CRB000722_Unable_to_instantiate_resolve_RootPOA", args,
                      e);
            throw new ServiceCreationException(
                    "CRB000722_Unable_to_instantiate_resolve_RootPOA", args, e);
        }
        
        // registering value factory for value types
        
        Map<String, java.lang.Object> map = consumerServiceDescriptor
          .getServerCorbaClassesHolder().getValueTypeIdAndInstance();

        if (map == null || map.size() == 0) {
          LOG.debug("CRB000130_No_value_type_factory_to_register_using_the_ORB");
        } else {
          LOG.debug("ValueTypeFactoryMap.size=" + map.size());

          // registering all valuetypes factories.
          for (String id : map.keySet()) {
            ValueFactory vf = (ValueFactory) map.get(id);

            LOG.debug("Registering a ValueType Factory. id=" + id
              + "; instance=" + vf);

            ((org.omg.CORBA_2_3.ORB) orb).register_value_factory(id, vf);
          }
        }        

        Constructor constructor;
        Class[] classes = new Class[] {
                serverCorbaClassesHolder.getCorbaOperations(), POA.class };
        try {
            Class clazz = serverCorbaClassesHolder.getCorbaPOATie();

            constructor = clazz.getConstructor(classes);
        } catch (SecurityException e) {
            Object[] args = new Object[] {
                    serverCorbaClassesHolder.getCorbaPOATie(),
                    Arrays.toString(classes), e.getMessage() };

            LOG.error("CRB000723_Unable_to_create_constructor", args, e);
            throw new ServiceCreationException(
                    "CRB000723_Unable_to_create_constructor", args, e);
        } catch (NoSuchMethodException e) {
            Object[] args = new Object[] {
                    serverCorbaClassesHolder.getCorbaPOATie(),
                    Arrays.toString(classes), e.getMessage() };

            LOG.error("CRB000723_Unable_to_create_constructor", args, e);
            throw new ServiceCreationException(
                    "CRB000723_Unable_to_create_constructor", args, e);
        }
        Servant poaTie;
        Object[] arguments = new Object[] { poaDelegate, rootPoa };
        try {
            poaTie = (Servant) constructor.newInstance(arguments);
        } catch (IllegalArgumentException e) {
            Object[] args = new Object[] {
                    constructor, Arrays.toString(arguments), e.getMessage() };

            LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
            throw new ServiceCreationException(
                    "CRB000724_Unable_to_instatiate_object", args, e);
        } catch (InstantiationException e) {
            Object[] args = new Object[] {
                    constructor, Arrays.toString(arguments), e.getMessage() };

            LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
            throw new ServiceCreationException(
                    "CRB000724_Unable_to_instatiate_object", args, e);
        } catch (IllegalAccessException e) {
            Object[] args = new Object[] {
                    constructor, Arrays.toString(arguments), e.getMessage() };

            LOG.error("CRB000724_Unable_to_instatiate_object", args, e);
            throw new ServiceCreationException(
                    "CRB000724_Unable_to_instatiate_object", args, e);
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            Object[] args = new Object[] { constructor,
                    Arrays.toString(arguments), cause.getMessage() };

            LOG.error("CRB000725_Exception_calling_constructor", args, cause);
            throw new ServiceCreationException(
                    "CRB000725_Exception_calling_constructor", args, cause);
        }
        consumerServiceDescriptor.setPoaTie(poaTie);
    }
//------------------------------------------------------------------------------ PRE

    /**
     * @param consumerServiceDescriptor    The consumer service descriptor
     * @throws ServiceActivationException  The service activation exception
     */
    public void registerAndActivateService(
            ConsumerServiceDescriptor consumerServiceDescriptor)
            throws ServiceActivationException {
        Servant poaTie = consumerServiceDescriptor.getPoaTie();
        POA rootPoa = poaTie._default_POA();
        ORB orb = consumerServiceDescriptor.getOrb();

        LOG.debug("Trying to activate POA");
        try {
            rootPoa.the_POAManager().activate();
        } catch (AdapterInactive e) {
            Object[] args = new Object[] { rootPoa, e.getMessage() };

            LOG.error("CRB000726_Unable_to_activate_RootPOA", args, e);
            throw new ServiceActivationException(
                    "CRB000726_Unable_to_activate_RootPOA", args, e);
        }
        org.omg.CORBA.Object servantRef = poaTie._this_object(orb);
        LOG.debug("POA TYPE CLASS -------->"+poaTie.getClass());
        LOG.debug("Servant Ref -------->"+servantRef.toString());
        // get the root naming context
        org.omg.CORBA.Object objRef;
        try {
            LOG.debug("Trying to resolve the NameService");
            objRef = orb.resolve_initial_references("NameService");
        } catch (InvalidName e) {
            Object[] args = new Object[] { orb, e.getMessage() };

            LOG.error("CRB000727_Unable_to_resolve_inital_reference", args, e);
            throw new ServiceActivationException(
                    "CRB000727_Unable_to_resolve_inital_reference", args, e);
        }
        // Use NamingContextExt which is part of the Interoperable
        // Naming Service specification.
        NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
        String name = consumerServiceDescriptor.getCorbaServiceName();
        LOG.debug("Trying to registering the service with name: " + name);
        if (name == null || "".equals(name)) {
            LOG.error("CRB000728_Trying_to_register_service", name);
            throw new ServiceActivationException(
                    "CRB000728_Trying_to_register_service",
                    new Object[] { name }, null);
        }
        // bind the Object Reference in Naming
        NameComponent[] path;
        try {
            path = ncRef.to_name(name);
        } catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
            Object[] args = new Object[] { name, ncRef, e.getMessage() };

            LOG.error("CRB000729_Invalid_name", args, e);
            throw new ServiceActivationException("CRB000729_Invalid_name",
                                                 args, e);
        }
        try {
            LOG.debug("Trying to rebind the servant");
            ncRef.rebind(path, servantRef);
        } catch (NotFound e) {
            Object[] args = new Object[] {
                    servantRef, path, ncRef, e.getMessage() };

            LOG.error("CRB000730_Not_found_registering_servant", args, e);
            throw new ServiceActivationException(
                    "CRB000730_Not_found_registering_servant", args, e);
        } catch (CannotProceed e) {
            Object[] args = new Object[] {
                    servantRef, path, ncRef, e.getMessage() };

            LOG.error("CRB000731_Cannot_proceed_registering_servant", args, e);
            throw new ServiceActivationException(
                    "CRB000731_Cannot_proceed_registering_servant", args, e);
        } catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
            Object[] args = new Object[] {
                    servantRef, path, ncRef, e.getMessage() };

            LOG.error("CRB000732_Invalid_name_registering_servant", args, e);
            throw new ServiceActivationException(
                    "CRB000732_Invalid_name_registering_servant", args, e);
        }
    }
}
