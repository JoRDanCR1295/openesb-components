package it.imolinfo.jbi4corba.test.webservice.generator.complexintType;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/complexintType/Persona.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from Test-IntType-Complex-multInterface.idl
* luned� 19 gennaio 2009 9.37.07 CET
*/

public final class Persona implements org.omg.CORBA.portable.IDLEntity
{
  public it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo echopersona = null;
  public String nome = null;
  public String cognome = null;

  public Persona ()
  {
  } // ctor

  public Persona (it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo _echopersona, String _nome, String _cognome)
  {
    echopersona = _echopersona;
    nome = _nome;
    cognome = _cognome;
  } // ctor

} // class Persona