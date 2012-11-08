 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.webservice.generator.AnyType;
import it.imolinfo.jbi4corba.webservice.generator.AnyTypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceTypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.UnionType;
import it.imolinfo.jbi4corba.webservice.generator.UnionTypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.Util;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;


import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.TraceMethodVisitor;

/**
 * bytecode manipulation.
 *
 * For each Object visited we modify the bytecode as following:
 *
 * 1) for each public or protected member we add the getter and setter method.
 * 2) if the class is 'abstract' we remove this keyword.
 * 3) we add/modify a default constructor where we istantiate the array
 *    (if the class contains one or more array) to avoid NullPointerException.
 * 4) We add the jaxb Annotations
 *
 */
public class IdlToWsdlAdapter extends ClassAdapter {

  /**
   * Logger.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(IdlToWsdlAdapter.class);
  
  public static final String JAXB_XML_ACCESSOR_TYPE  = "Ljavax/xml/bind/annotation/XmlAccessorType;";
  
  public static final String JAXB_XML_ACCESS_TYPE = "Ljavax/xml/bind/annotation/XmlAccessType;";
  
  public static final String JAXB_XML_ELEMENT = "Ljavax/xml/bind/annotation/XmlElement;";   
    
  public static final String PROPERTY_ACCESS_TYPE = "PROPERTY";
  
  public static final String NONE_ACCESS_TYPE = "NONE";
  
  public static final String FIELD_ACCESS_TYPE = "FIELD";
  
  protected String className = null;
  
  protected ClassWriter classWriter = null;

  protected Map<String, String> mapOfFields = new HashMap<String, String>();

  // Utility Object
  private ByteCodeManipulationUtil bcmUtil = new ByteCodeManipulationUtil();
  
  private boolean areClassAnnotationsPresent = false;
  
  // True if the class is a Programmatic Exception
  private boolean isException = false;

  private Map<String, UnionType> allUnionTypes;
  
  private Map<String, String> fieldsUnionTypes;
  
  private Map<String, InterfaceType> allInterfaceTypes;
  
  private Map<String, String> fieldsInterfaceTypes;
  
  private boolean excludeType;
  
  
  
  /**
   * The adapater used to manipulate the code.
   *
   * @param   cv    The ClassVisitor used in this object.
   * @param   cw    The ClassWriter used in this object.
   * @param   cn    The class name to manipulate.
   */
	public IdlToWsdlAdapter(ClassVisitor cv, ClassWriter cw, String cn,
			Map<String, UnionType> allUnTypes, Map<String, String> fUnTypes,
			Map<String, InterfaceType> allInTypes,Map<String, String> fIntTypes,boolean isEx,boolean excludeType) {
    super(cv);

    classWriter = cw;
    className = cn;
    isException = isEx;    
    allUnionTypes = allUnTypes;
    fieldsUnionTypes = fUnTypes;
    fieldsInterfaceTypes = fIntTypes;
    allInterfaceTypes=allInTypes;
    this.excludeType=excludeType;
    
		
    LOG.debug("CRB000604_new_GetterSetterAdapter", new Object[]{cv, cw, cn, allUnionTypes, isEx});
  }

  /**
   * Override.
   * @param version  	   The method visitor
   * @param access         The access
   * @param name           The name
   * @param signature      The signature
   * @param superName      The super name
   * @param interfaces     The interfaces
   */
  @Override
  public void visit(int version,
                    int access,
                    String name,
                    String signature,
                    String superName,
                    String [] interfaces) {
    LOG.debug(">>>>> visit - begin");

    LOG.debug("CRB000603_VISIT", new Object[]{version, access, name, signature, superName, interfaces});

    if (Opcodes.ACC_INTERFACE == access) {

        LOG.debug("This class (" + name + ") is an interface.");
        
    } 
   
    if ((Opcodes.ACC_ABSTRACT & access) == Opcodes.ACC_ABSTRACT) {

        access = access & (~ Opcodes.ACC_ABSTRACT);

        LOG.debug("This class (" + name + ") was an abstract "
                + "... now is a concrete class.");

    } else {
        LOG.debug("This class (" + name + ") is a concrete class.");
    }
    
    // Remove the "final"
    if ((Opcodes.ACC_FINAL & access) == Opcodes.ACC_FINAL) {
        access = access & (~Opcodes.ACC_FINAL);
        LOG.debug("Removed final from class");
    }

    // if (superName.equals("org/omg/CORBA/UserException")) {
    if (isException) {
        // superName = "java/lang/Exception";       
        LOG.debug("The class: " + name +" is an Exception");
    } else {
        LOG.debug("The class: " + name +" is not an Exception");
    }    
    
    LOG.debug("<<<<< visit - end");
        
    super.visit(version, access, name, signature, superName, interfaces);
    
    addAnnotation();  
  }
  
  /**Visit
   * Adds the @XmlAccessorType(XmlAccessType.PROPERTY) jaxb annotation.
   * See CRB-154
   * @param name
   */
  private void addAnnotation() {
      if (!areClassAnnotationsPresent) {
          
          // Adds the XMLAccessorTypeAnnotation
          AnnotationVisitor av = null;          
          
          // We annotate the classes with field access type
          av = cv.visitAnnotation(JAXB_XML_ACCESSOR_TYPE, true);
          //the Annotations was changed for generate WSDL with Union Type
          av.visitEnum("value", JAXB_XML_ACCESS_TYPE, FIELD_ACCESS_TYPE);          
          
          if (av != null) {
              av.visitEnd();
          }                
          
          areClassAnnotationsPresent = true;
      }
  }

  /**
   * Override.
   * @param access         The access
   * @param name           The name
   * @param desc           The description
   * @param signature      The signature
   * @param exceptions     The exceptions
   * @return               The return
   */
  @Override
  public MethodVisitor visitMethod(int access,
          String name,
          String desc,
          String signature,
          String[] exceptions) {

    LOG.debug(">>>>> visitMethod - begin");

    LOG.debug("visitMethod. access="     + access
                        + "; name="       + name
                        + "; desc="       + desc
                        + "; signature="  + signature
                        + "; exceptions=" + exceptions);
       
    
    // if we found the default constructor then ...
    
    // replace union and any types in method signature with java.lang.Object
    try {
    	if (!excludeType)
        {
    		desc = UnionTypeUtils.replaceUnionTypesInMethodSignature(desc, allUnionTypes, null);
    		desc = AnyTypeUtils.replaceAnyTypesInMethodSignature(desc);
        }
	} catch (ClassGenerationException e) {
		// nothing to replace
		LOG.debug("visitMethod - Nothing to replace:" + e.getMessage());
	}
	TraceMethodVisitor mv = null;
    if ("<init>".equals(name)) {

        LOG.debug("default constructor modifications.");
               
        mv = new AppenderMethodVisitor(
                super.visitMethod(access, name, desc, signature, exceptions),
                mapOfFields,
                className);
             
        LOG.debug("<<<<< visitMethod - end. AppenderMethodVisitor=" + mv);
       
       
      }

    // else
    LOG.debug("<<<<< visitMethod - end. "
            + "methodName=" + name + "; methodDescription=" + desc);
    // replace attribution instructions of union and any types
    if (!excludeType)
    {
	    if (mv == null)
	    	mv = new ReplaceUnionsMethodVisitor( super.visitMethod(access, name, desc, signature, exceptions),allUnionTypes);
	    else
	    	mv = new ReplaceUnionsMethodVisitor( mv,allUnionTypes);
	    
	    if (mv == null)
	    	mv = new ReplaceAnyMethodVisitor( super.visitMethod(access, name, desc, signature, exceptions));
	    else
	    	mv = new ReplaceAnyMethodVisitor( mv);
    }
    else
    {
    	if (mv == null)
	    	return super.visitMethod(access, name, desc, signature, exceptions);
    }
    return mv;
  }

/**
   * @param access      the field's access flags (see Opcodes).
   *                    This parameter also indicates if the field is synthetic
   *                    and/or deprecated.
   * @param name        the field's name.
   * @param desc        the field's descriptor (see Type).
   * @param signature    the field's signature. May be null if the field's type
   *                    does not use generic types.
   * @param value        the field's initial value.
   *                    This parameter, whitweakIdlToWsdlClassesch may be null if the field does not
   *                    have an initial value, must be an Integer, a Float,
   *                    a Long, a Double or a String (for int, float, long or
   *                    String fields respectively).
   *                    This parameter is only used for static fields.
   *                    Its value is IGNORED for non static fields,
   *                    which must be initialized through bytecode
   *                    instructions in constructors or methods.
   *
   * @return    a visitor to visit field annotations and attributes,
   *             or null if this class visitor is not interested
   *             in visiting these annotations and attributes.
   */
  @Override
  public FieldVisitor visitField(
        int access,
          String name,
          String desc,
          String signature,
          Object value) {

    LOG.debug(">>>>> visitField - begin:" + desc);

    LOG.debug("visitField. access="    + access
                        + "; name="      + name
                        + "; desc="      + desc
                        + "; signature=" + signature
                        + "; value="     + value);
    
	boolean isArray = TypeUtils.isArray(desc);
	String arrayStr = TypeUtils.getArrayDimmentionAsPrefix(desc);
	UnionType union =UnionTypeUtils.isUnionType(desc, isArray, allUnionTypes);
	InterfaceType intf = InterfaceTypeUtils.isInterfaceType(desc, isArray, allInterfaceTypes);
	boolean isAny = TypeUtils.getTypeFromTypeDescription(desc).equals(AnyType.CORBA_ANY_TYPE);
        //If excludeType is true the bytecode manipulation about Type as Any,Interfacce or Union is Disable
        
	if (isAny  && !excludeType) {
		desc = arrayStr + "Ljava/lang/Object;";
	}
	if ((union != null)  && !excludeType) {
		desc = arrayStr + "L" + union.getTypeName().replace('.', '/') + "Wrapper;";
	}
	
	if (intf != null && !excludeType) {
		if (isArray)
			desc = Type.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference[].class);
		else
			desc = Type.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference.class);
	}

    if (Opcodes.ACC_PUBLIC == access || Opcodes.ACC_PROTECTED == access) {
        bcmUtil.createSetter(classWriter, className, name, desc);        
        
        //bcmUtil.createGetter(classWriter, className, name, desc, isException);
        bcmUtil.createGetter(classWriter, className, name, desc, false);

        mapOfFields.put(name, desc);
    }
    LOG.debug("<<<<< visitField - end");

		FieldVisitor fv = super
				.visitField(access, name, desc, signature, value);

		if (union != null && !excludeType) {
	//		UnionTypeUtils.addUnionTypeAnnotation(fv, union, allUnionTypes);
			fieldsUnionTypes.put(name, union.getTypeName());
		}

		//Save metadata about InterfaceType field   
		if (intf != null && !excludeType) {
			
			fieldsInterfaceTypes.put(name,intf.getTypeName());
		}
		if (!ignoreField(name, access))
		{
			AnnotationVisitor xmlElem = fv.visitAnnotation(JAXB_XML_ELEMENT, true);
			xmlElem.visit("required", true);
			xmlElem.visit("nillable", false);
			
			if (xmlElem != null) {
	      	  xmlElem.visitEnd();
	        }
		}
		return fv;
	}
  	
   /**
	 * ignoreField
	 * 
	 * @param fieldName
	 * @param accessModif
	 * @return
	 */
   private static boolean ignoreField(String fieldName, int accessModif)
	{
		
		//treat value type case(ignore field private static String[] _truncatable_ids)
		if (fieldName.equals("_truncatable_ids") && Modifier.isPrivate(accessModif) 
				&& Modifier.isStatic(accessModif))
		{
			return true;
		}
		
		return false;
	}
	
  /**
  * XXX javadoc.
  */
  @Override
  public void visitEnd() {
    bcmUtil.createToString(classWriter);
    bcmUtil.createEquals(classWriter);

    super.visitEnd();
  }

}
