 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.jbi.cxf;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.xpath.XPathConstants;

import org.apache.cxf.common.util.StringUtils;
import org.apache.cxf.databinding.DataBinding;
import org.apache.cxf.databinding.DataReader;
import org.apache.cxf.helpers.CastUtils;
import org.apache.cxf.helpers.DOMUtils;
import org.apache.cxf.helpers.XPathUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.jaxb.JAXBDataBinding;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.model.BindingOperationInfo;
import org.apache.cxf.service.model.FaultInfo;
import org.apache.cxf.service.model.MessagePartInfo;
import org.apache.cxf.staxutils.StaxUtils;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Intercaptor that converts the XML to Exceptions. Taken from the CXF
 * ClientFaultConverter class (fied some jaxb issue with our exceptions).
 * 
 * @author marco.
 */
@SuppressWarnings("unchecked")
public class Jbi4CorbaConsumerExceptionInterceptor extends
		AbstractPhaseInterceptor<Message> {

	/**
	 * Logger.
	 */
	private static final transient Logger LOG = LoggerFactory
			.getLogger(Jbi4CorbaConsumerExceptionInterceptor.class);

    /** The messages. */
    @SuppressWarnings("unused")
	private static final Messages MESSAGES = 
    	Messages.getMessages(Jbi4CorbaConsumerExceptionInterceptor.class);

	public Jbi4CorbaConsumerExceptionInterceptor() {
		super(Phase.UNMARSHAL);
	}

	public Jbi4CorbaConsumerExceptionInterceptor(String phase) {
		super(phase);
	}

	public void handleMessage(Message msg) {
		Fault fault = (Fault) msg.getContent(Exception.class);

		if (fault.getDetail() != null) {
			processFaultDetail(fault, msg);
			setStackTrace(fault, msg);
		}
	}

	protected void processFaultDetail(Fault fault, Message msg) {
		Element exDetail = (Element) DOMUtils.getChild(fault.getDetail(),
				Node.ELEMENT_NODE);
		if (exDetail == null) {
			return;
		}
		QName qname = new QName(exDetail.getNamespaceURI(), exDetail
				.getLocalName());
		FaultInfo faultWanted = null;
		MessagePartInfo part = null;
		BindingOperationInfo boi = msg.getExchange().get(
				BindingOperationInfo.class);
		if (boi == null) {
			return;
		}
		if (boi.isUnwrapped()) {
			boi = boi.getWrappedOperation();
		}
		for (FaultInfo faultInfo : boi.getOperationInfo().getFaults()) {
			for (MessagePartInfo mpi : faultInfo.getMessageParts()) {
				if (qname.equals(mpi.getConcreteName())) {
					faultWanted = faultInfo;
					part = mpi;
					break;
				}
			}
			if (faultWanted != null) {
				break;
			}
		}
		if (faultWanted == null) {
			// did not find it using the proper qualified names, we'll try again
			// with just the localpart
			for (FaultInfo faultInfo : boi.getOperationInfo().getFaults()) {
				for (MessagePartInfo mpi : faultInfo.getMessageParts()) {
					if (qname.getLocalPart().equals(
							mpi.getConcreteName().getLocalPart())) {
						faultWanted = faultInfo;
						part = mpi;
						break;
					}
				}
				if (faultWanted != null) {
					break;
				}
			}
		}
		if (faultWanted == null) {
			return;
		}
		Service s = msg.getExchange().get(Service.class);
		DataBinding dataBinding = s.getDataBinding();

		Object e = null;
		if (isDOMSupported(dataBinding)) {
			DataReader<Node> reader = dataBinding.createReader(Node.class);
			reader.setProperty(DataReader.FAULT, fault);
			JAXBContext context = ((JAXBDataBinding) dataBinding).getContext();
			try {
				Unmarshaller unmarshaller = context.createUnmarshaller();
				e = unmarshallException(unmarshaller, exDetail, part);
				
			} catch (Exception ex) {
				if (ex instanceof javax.xml.bind.UnmarshalException) {
					javax.xml.bind.UnmarshalException unmarshalEx = (javax.xml.bind.UnmarshalException) ex;
					throw new Fault(unmarshalEx);
				} else {
					throw new Fault(ex);
				}
			}
		}

		LOG.debug("Returned object of class:" + e.getClass().getName());
		if (!(e instanceof Exception)) {

			try {
				Class<?> exClass = faultWanted.getProperty(Class.class
						.getName(), Class.class);
				LOG.debug("ExClass exClass:" + exClass.getName());				
				if (e == null) {
					Constructor constructor = exClass
							.getConstructor(new Class[] { String.class });
					e = constructor.newInstance(new Object[] { fault
							.getMessage() });
				} else {				    
					Constructor constructor = getConstructor(exClass, e);
					e = constructor.newInstance(new Object[] {
							fault.getMessage(), e });
					LOG.debug("After newInstance");  
				}
				msg.setContent(Exception.class, e);
			} catch (Throwable e1) {
			    e1.printStackTrace();
				String errmsg = MESSAGES.getString("CRB000901_Exception_while_creating_exception");
				LOG.debug(errmsg, e1.getMessage());
			}
		} else if (e != null) {
			if (fault.getMessage() != null) {
				Field f;
				try {
					f = Throwable.class.getDeclaredField("detailMessage");
					f.setAccessible(true);
					f.set(e, fault.getMessage());
				} catch (Exception e1) {
					String errmsg = MESSAGES.getString("CRB000902_Exception_in_fault_processing");
					LOG.debug(errmsg, e1.getMessage());
				}
			}
			msg.setContent(Exception.class, e);
		}
	}

	private Constructor getConstructor(Class<?> faultClass, Object e)
			throws NoSuchMethodException {
		Class<?> beanClass = e.getClass();
		Constructor cons[] = faultClass.getConstructors();
		for (Constructor c : cons) {
			if (c.getParameterTypes().length == 2
					&& String.class.equals(c.getParameterTypes()[0])
					&& c.getParameterTypes()[1].isInstance(e)) {
			    LOG.debug("Found constructor: " + c);
			    LOG.debug("The second parameter type is: " + c.getParameterTypes()[1]);			    
				return c;
			}
		}
		try {
		    LOG.debug("Found constructor directly");
			return faultClass.getConstructor(new Class[] { String.class,
					beanClass });
		} catch (NoSuchMethodException ex) {
		    ex.printStackTrace();
			Class<?> cls = getPrimitiveClass(beanClass);
			if (cls != null) {
			    LOG.debug("Uses the primitive class:  " + cls);
				return faultClass.getConstructor(new Class[] { String.class,
						cls });
			} else {
				throw ex;
			}
		}

	}

	private boolean isDOMSupported(DataBinding db) {
		boolean supportsDOM = false;
		for (Class c : db.getSupportedReaderFormats()) {
			if (c.equals(Node.class)) {
				supportsDOM = true;
			}
		}
		return supportsDOM;
	}

	private void setStackTrace(Fault fault, Message msg) {
		Map<String, String> ns = new HashMap<String, String>();
		XPathUtils xu = new XPathUtils(ns);
		String ss = (String) xu.getValue("/" + Fault.STACKTRACE + "/text()",
				fault.getDetail(), XPathConstants.STRING);
		List<StackTraceElement> stackTraceList = new ArrayList<StackTraceElement>();
		if (!StringUtils.isEmpty(ss)) {
			StringTokenizer st = new StringTokenizer(ss, "\n");
			while (st.hasMoreTokens()) {
				String oneLine = st.nextToken();
				StringTokenizer stInner = new StringTokenizer(oneLine, "!");
				StackTraceElement ste = new StackTraceElement(stInner
						.nextToken(), stInner.nextToken(), stInner.nextToken(),
						Integer.parseInt(stInner.nextToken()));
				stackTraceList.add(ste);
			}
			if (stackTraceList.size() > 0) {
				StackTraceElement[] stackTraceElement = new StackTraceElement[stackTraceList
						.size()];
				Exception e = msg.getContent(Exception.class);
				e.setStackTrace(stackTraceList.toArray(stackTraceElement));
			}
		}

	}

	private Class<?> getPrimitiveClass(Class<?> cls) {
		if (cls.isPrimitive()) {
			return cls;
		}
		try {
			Field field = cls.getField("TYPE");
			Object obj = (Object) cls;
			Object type = field.get(obj);
			if (type instanceof Class) {
				return (Class) type;
			}
		} catch (Exception e) {
			String errmsg = MESSAGES.getString("CRB000902_Exception_in_fault_processing");
			LOG.error(errmsg, e.getMessage());
		}
		return null;
	}

	/**
	 * Unmarshall the exception (from XML to Java).
	 * @param u
	 * @param source
	 * @param part
	 * @return
	 */
	public static Object unmarshallException(Unmarshaller u, Object source,
			MessagePartInfo part) {
		XMLStreamReader reader = null;
		Object obj = null;
		try {
			if (source instanceof XMLStreamReader) {
				reader = (XMLStreamReader) source;
			} else if (source instanceof Element) {
				reader = StaxUtils.createXMLStreamReader((Element) source);
				try {
					// advance into the node
					reader.nextTag();
				} catch (XMLStreamException e) {
					String errmsg = MESSAGES.getString("CRB000903_Exception_in_exception_unmarshalling");
					LOG.error(errmsg, e.getMessage());
				}
			} else {
				String errmsg = MESSAGES.getString("CRB000904_Wrong_source_type_in_Exception_unmarshalling");
				LOG.error(errmsg);
			}

			QName qn = part.getElementQName();
			if (!qn.equals(reader.getName())) {
				String errmsg = MESSAGES.getString("CRB000905_Element_name_mismatch_in_Exception_unmarshalling");
				LOG.error(errmsg);
			}

			Class<?> cls = part.getTypeClass();

			try {
				Constructor cons = cls.getConstructor();
				obj = cons.newInstance();
			} catch (NoSuchMethodException nse) {
				Constructor cons = cls
						.getConstructor(new Class[] { String.class });
				obj = cons.newInstance(new Object[1]);
			}

			XmlAccessorType accessorType = cls
					.getAnnotation(XmlAccessorType.class);
			if (accessorType == null && cls.getPackage() != null) {
				accessorType = cls.getPackage().getAnnotation(
						XmlAccessorType.class);
			}
			//XmlAccessType accessType = accessorType != null ? accessorType
			//		.value() : XmlAccessType.PUBLIC_MEMBER;
			reader.nextTag();
			while (reader.getEventType() == XMLStreamReader.START_ELEMENT) {
				QName q = reader.getName();

				String s = Character.toUpperCase(q.getLocalPart().charAt(0))
						+ q.getLocalPart().substring(1);
				Method m = null;
				try {
					m = cls.getMethod("get" + s);
				} catch (NoSuchMethodException mex) {
					m = cls.getMethod("is" + s);
				}
				Type type = m.getGenericReturnType();
				Method m2 = cls.getMethod("set" + s, m.getReturnType());
				if (isArray(type)) {
					Class<?> compType = getArrayComponentType(type);
					List<Object> ret = unmarshallArray(u, reader, q, compType,
							createList(type));
					Object o = ret;
					if (!isList(type)) {
						if (compType.isPrimitive()) {
							o = java.lang.reflect.Array.newInstance(compType,
									ret.size());
							for (int x = 0; x < ret.size(); x++) {
								Array.set(o, x, ret.get(x));
							}
						} else {
							o = ret.toArray((Object[]) Array.newInstance(
									compType, ret.size()));
						}
					}

					m2.invoke(obj, o);
				} else {
					Object o = getElementValue(u.unmarshal(reader, m
							.getReturnType()));
					m2.invoke(obj, o);
				}
			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return obj;

	}

	static boolean isArray(Type cls) {
		if (cls instanceof Class) {
			return ((Class) cls).isArray();
		} else if (cls instanceof ParameterizedType) {
			return true;
		} else if (cls instanceof GenericArrayType) {
			return true;
		}
		return false;
	}

	static Class<?> getArrayComponentType(Type cls) {
		if (cls instanceof Class) {
			if (((Class) cls).isArray()) {
				return ((Class) cls).getComponentType();
			} else {
				return (Class) cls;
			}
		} else if (cls instanceof ParameterizedType) {
			for (Type t2 : ((ParameterizedType) cls).getActualTypeArguments()) {
				return getArrayComponentType(t2);
			}
		} else if (cls instanceof GenericArrayType) {
			GenericArrayType gt = (GenericArrayType) cls;
			Class ct = (Class) gt.getGenericComponentType();
			return Array.newInstance(ct, 0).getClass();
		}
		return null;
	}

	private static List<Object> createList(Type genericType) {
		if (genericType instanceof ParameterizedType) {
			Type tp2 = ((ParameterizedType) genericType).getRawType();
			if (tp2 instanceof Class) {
				Class<?> cls = (Class) tp2;
				if (!cls.isInterface()
						&& List.class.isAssignableFrom((Class<?>) cls)) {
					try {
						return CastUtils.cast((List) cls.newInstance());
					} catch (Exception e) {
					    // ignore, just return an ArrayList
	    				String errmsg = MESSAGES.getString("CRB000906_Exception_in_list_exception_unmarshalling");
	    				LOG.warn(errmsg, e.getMessage());
					}
				}
			}
		}
		return new ArrayList<Object>();
	}

	public static List<Object> unmarshallArray(Unmarshaller u, Object source,
			QName elName, Class<?> clazz, List<Object> ret) throws Exception {
		try {
			XMLStreamReader reader = null;
			if (source instanceof XMLStreamReader) {
				reader = (XMLStreamReader) source;
			} else if (source instanceof Element) {
				reader = StaxUtils.createXMLStreamReader((Element) source);
			} else {
				// empty...
				String errmsg = MESSAGES.getString("CRB000907_Should_be_XMLStreamReader_or_Element");
				LOG.error(errmsg);
			}
			while (reader.getName().equals(elName)) {
				Object obj = u.unmarshal(reader, clazz);
				if (obj instanceof JAXBElement) {
					obj = ((JAXBElement) obj).getValue();
				}
				ret.add(obj);
				while (reader.getEventType() != XMLStreamConstants.START_ELEMENT
						&& reader.getEventType() != XMLStreamConstants.END_ELEMENT) {
					reader.nextTag();
				}
			}
			return ret;
		} catch (Fault ex) {
			ex.fillInStackTrace();
			throw ex;
		} catch (Exception ex) {
			if (ex instanceof javax.xml.bind.UnmarshalException) {
				javax.xml.bind.UnmarshalException unmarshalEx = (javax.xml.bind.UnmarshalException) ex;
				throw new Fault(unmarshalEx);

			} else {
				throw new Fault(ex);
			}
		}
	}

	private static boolean isList(Type cls) {
		if (cls instanceof ParameterizedType) {
			return true;
		}
		return false;
	}

	public static Object getElementValue(Object obj) {
		if (null == obj) {
			return null;
		}

		if (obj instanceof JAXBElement) {
			return ((JAXBElement<?>) obj).getValue();
		}
		return obj;
	}

}
