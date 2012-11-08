package it.imolinfo.jbi4corba.webservice.generator.typedef;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlTransient;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;
import org.omg.CORBA.Any;

/**
 * Complex TypeDef class generator.
 * 
 * The class generated is:
 *
 * <pre>
 * {@code
 *
 * @XmlAccessorType(XmlAccessType.FIELD)
 * public class MyComplexType extends ComplexType {
 *   // @XmlTransient
 *   // final static public MyComplexTypeHelper helperClass= MyComplexTypeHelper.class;
 * }
 * }
 * </pre>
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
@SuppressWarnings("unchecked")
public class ComplexTypeDef extends TypeDef {
	
	private static final String CLASS_EXTENSION = ".class";
	
	/**
	 * Class logger
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(ComplexTypeDef.class);	
	
	protected ComplexTypeDef(String className, String helperClassName, Class aliasedClass, String id) {
		super(className, helperClassName, aliasedClass, id);		
	}
	
	/**
	 * Removes the "Final" keyword from the Complex class (otherwise cannot be extended).
	 * @param classesDir
	 * @throws ClassGenerationException 
	 */
	private void changeFinalAliasedClass(String classesDir) throws ClassGenerationException {
		String filePath = aliasedClass.getName().replace('.', File.separatorChar).concat(CLASS_EXTENSION);
		
		// Absolute file path 
	    String absFilePath = classesDir + File.separator + filePath;	 
	    	    	   
	    ClassWriter  cw = new ClassWriter(true); // visitMaxs
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));	
        RemoveFinalAdapter rfa = new RemoveFinalAdapter(tv);
                       
        ClassReader cr = Util.getAsmCLassReader(absFilePath);
        cr.accept(rfa, true);
        byte [] newBytecode = cw.toByteArray();
		
		LOG.debug("Class to change: " + absFilePath);
        Util.saveAsJavaClass(absFilePath, newBytecode);		
	}
	
	public void createTypeDefClass(String classesDir) throws ClassGenerationException {
		
		// Removed the final keyword on the aliased class
		changeFinalAliasedClass(classesDir);
		
		// I have to calculate this values (i cannot load the class...doesn't exist yet)
		String classInternalName = this.getClassName().replace('.','/');
		String aliasedclassInternalName =  Type.getInternalName(aliasedClass);
				
		// helper class
		Class helperClass = this.getHelperClass(classesDir);  		
		String helperDescriptor = Type.getDescriptor(helperClass);	
				
		// Class, Object and jaxb annotations
		String javaLangClassDescriptor = Type.getDescriptor(Class.class);
		String xmlAccessorTypeDescriptor = Type.getDescriptor(XmlAccessorType.class);
		String xmlAccessTypeDescriptor = Type.getDescriptor(XmlAccessType.class);
		String xmlTransientDescriptor = Type.getDescriptor(XmlTransient.class);
										
		ClassWriter cw = new ClassWriter(true);
		
		// The class, extends the aliased class
		cw.visit(Opcodes.V1_5, ACC_PUBLIC + ACC_SUPER, classInternalName ,null, aliasedclassInternalName, null);
		
		// The class XmlAccessorType Annotation:	@XmlAccessorType(XmlAccessType.FIELD)
		AnnotationVisitor av = cw.visitAnnotation(xmlAccessorTypeDescriptor, true);		  
        //the Annotations was changed for generate WSDL with Union Type
        av.visitEnum("value", xmlAccessTypeDescriptor, "FIELD"); 
		av.visitEnd();	
		
		/* 
		 * @XmlTransient
		 * final static private Class helperClass = null; 		// cannot be inited (null) 	
		 */	
		/*
		FieldVisitor fvHelperClass =  cw.visitField(ACC_PRIVATE + ACC_STATIC + ACC_FINAL
				, "helperClass", javaLangClassDescriptor, null, null);
		AnnotationVisitor avHelper = fvHelperClass.visitAnnotation(xmlTransientDescriptor, true);
		avHelper.visitEnd();			
		fvHelperClass.visitEnd();
		*/
											
		// Class init. Inits the static "helperClass" field.	
		/*
		MethodVisitor mv = cw.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null);
		mv.visitCode();
		Label lclinit = new Label();
		mv.visitLabel(lclinit);
		mv.visitLineNumber(10, lclinit);
		mv.visitLdcInsn(Type.getType(helperDescriptor));		
		mv.visitFieldInsn(PUTSTATIC, classInternalName, "helperClass", javaLangClassDescriptor);
		Label lclinit1 = new Label();
		mv.visitLabel(lclinit1);
		mv.visitLineNumber(7, lclinit1);
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 0);
		mv.visitEnd();
		*/
		
		// Default constructor	
		MethodVisitor mvinit = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
		mvinit.visitCode();
		Label linit = new Label();
		mvinit.visitLabel(linit);
		mvinit.visitLineNumber(7, linit);
		mvinit.visitVarInsn(ALOAD, 0);
		mvinit.visitMethodInsn(INVOKESPECIAL, aliasedclassInternalName, "<init>", "()V");
		mvinit.visitInsn(RETURN);	
		mvinit.visitMaxs(1, 1);
		mvinit.visitEnd();		
				
		cw.visitEnd();
		byte[] bytecode = cw.toByteArray();
		
		String relativeClassName = getClassRelativeFileName();
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Writing out TypeDef class:" + classesDir + File.separatorChar + relativeClassName);
		}
		// Writes out the class
		Util.saveAsJavaClass(classesDir, relativeClassName, bytecode);		
	}	
	
	public String toString() {
		return "ComplexTypeDef: " + super.toString();
	}	

	public boolean isWrapped() {
		return false;
	}
	
	@Override
	public Object getTypeDefObject(ClassLoader classLoader, Object obj) throws ClassNotFoundException,
			InstantiationException, IllegalAccessException, SecurityException,
			NoSuchFieldException {
		 
		Class classDefinition = classLoader.loadClass(getClassName());		
        Object typeDefObject = classDefinition.newInstance();
        Field[] fields = obj.getClass().getFields();
        for (int i = 0; i < fields.length; i++) {
          fields[i].setAccessible(true);          
          // for each class/superclass, copy all fields
          // from this object to the clone
          fields[i].set(typeDefObject, fields[i].get(obj));          
        }                
        return typeDefObject;
	}	

	@Override
	public Object getAliasedObject(Object obj) throws SecurityException {
		// In this case, should be compatible (it's a subclass).
		return obj;
	}


}
