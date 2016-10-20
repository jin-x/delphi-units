{ DelphiVerDef7x.inc v1.00 (c) 2016 by Jin X (jin.x@sources.ru), http://xk7.ru/p/d/u }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// *** THIS INCLUDE IS DESIGNED FOR DELPHI COMPILERS VERSION 6 AND ABOVE !!! \\\ It outputs "__YOU_NEED_AT_LEAST_DELPHI_6_OR_ABOVE_TO_COMPILE_THIS__" error line to code for earler compiler versions! //
// *** This include cannot be used with Delphi 8 and 2007 for .NET compilers and will show fatal message in these cases !!!                                                                            //
// *** It's recommended to include this file right after "program" or "interface" keywords and other $DEFINE's declarations.                                                                           //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ===============
// CHANGES HISTORY
// ===============
// v1.00 beta (04.10.2016)
// [!] The first version of the include :)
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// This include defines symbols "IsDelphiXXX" and "DelphiXXX" where XXX is 6, 7, 2005, 2006, 2007, 2009, 2010, XE (and XE1), XE2..XE8, Seattle (and 10), Berlin (and 101).
// Symbol "IsDelphiXXX" is defined for exact version, "DelphiXXX" for specified and all above versions (e.g. "IsDelphi7" defined only for Delphi 7, "Delphi7" defined for Delphi 7 and above).
// Sumbols "NoDelphiXXX" means using earler version of compiler (you can also check it for incompatibility of exact compiler version). Equals to {$IFNDEF DelphiXXX}.
// Symbols "MOBILE" and "DESKTOP" are defined for mobile and desktop compilers. Symbols "IOS32" and "IOS64" are defined for 32 and 64-bit iOS platforms.
// Symbol "ANSI" is defined if "UNICODE" symbol is not defined.
// The following symbols are defined to indicate features (directives, attributes, types, etc.) support (see defines below for comments):
// * DelphiBerlin/Delphi101: Android601, UnsafeForAll, VolForAll (and VolatileForAll), WeakForAll, (and also UnsafeVolWeakForAll), MobUtf8String, MobRawByteString, Extended16 (for OS X only),
//     HelpNoPvtAcc (and HelpersNoPrivateAccess);
// * DelphiSeattle/Delphi10: Android511, iOS84;
// * DelphiXE8: FixedIntTypes, DblSizeLongTypes (for 64-bit iOS only), UnitScopedFQN (and UnitScopedFullyQualNames);
// * DelphiXE7: NewDynArrayOps, ParallelLib, NewCompIntr (and NewCompilerIntrin);
// * DelphiXE6: NewMobHPPEMIT;
// * DelphiXE5: ClassOpOverl (and ClassOperatorOverload - both for NextGen compiler);
// * DelphiXE4: Volatile, Weak, Ref, NoOldStrings (for iOS only), ARCModel, ZeroBasedStrings, ExtCompat (and ExtendedCompatibility);
// * DelphiXE3: RecHelpers, AtomicIntrin;
// * DelphiXE2: UnitScopeNames, CrossPlatformRTL, PackedByteAlign, Extended8 (for Win64 only);
// * DelphiXE: Align16, CodeAlign, StrongLinkTypes, RegExp, IngoreStringChecks;
// * Delphi2010: EnhancedRTTI, Attributes, Delayed, ClassConstr, ExtraInterfaceOps;
// * Delphi2009: DefaultIntrin, Generics, SmartPointers, AnonymMethods, NestedExcepts, PointerMath, ExitParam, TMonitor, DeprecatedText;
// * Delphi2007: [no symbols];
// * Delphi2006: RecMethods, OpOverl (and OperatorOverload), ClassVars, FastMM;
// * Delphi2005: ForIn, Inline, DynArrayInit, NestedTypes, NestedConst, ClassProperty, Unsafe, UnicodeIds, UsesWildcards, Region, ClassHelpers, Static, Strict, Sealed, Final,
//     AbsClass (and AbstractClasses), Experimental, ArrPropOverl (and ArrayPropertyOverload), MethodInfo;
// * Delphi7: WarnUnsafeCode, TFmtSetOverl (and TFormatSettingsOverloads).
// * Delphi6 and below: [no symbols].
// If some symbol is not defined "NoXXX" symbol is defined (NoForIn, NoInline, NoStatic, NoClassHelpers etc., except build-in but including NoMOBILE, NoDESKTOP, NoIOS32, NoIOS64, NoANSI, NoUNICODE).
// So you can use these symbols with $IFNDEF even if this file may be not included and you prefer to use some feature (e.g. {$IFNDEF NoInline}inline;{$ENDIF} instead of {$IFDEF Inline}inline;{$ENDIF}).
// p.s. "NoOldStrings" symbol is inverted to "OldStrings". Be careful to use {$IFDEF OldStrings} if this include file may be absent because this define will be absent too.
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// You may define one or more symbols "NeedDelphiXXX", "OnlyDelphiXXX" or "NotAboveDelphiXXX", "NotForDelphiXXX", "TestWarnings", "TesedDelphiXXX" and "DesignedForDelphiXXX" BEFORE including this file:
// * "NeedDelphiXXX" is used to prevent compilation by earler Delphi version (compiler will show fatal message); you shouldn't use "NeedDelphi6" because this include works only with Delphi 6 and above.
// * "OnlyDelphiXXX" is used to indicate that code can be compiled ONLY by Delphi version you need (compiler will show fatal message for other compiler versions);
// * "NotAboveDelphiXXX" is used to prevent compilation by later Delphi version (compiler will show fatal message);
// * "NotForDelphiXXX" is used to indicate incompatibility with some Delphi version (compiler will show fatal message for this compiler version);
// * "TestWarnings" with one or more "TestedDelphiXXX" are used to show warning messages for Delphi version compilers that are not well-tested with this code;
// * "DesignedForDelphiXXX" is used to indicate compiler version this code is best designed for (compiler will show warning messages for other versions);
//    you may define also "TestedDelphiXXX" symbols (without "TestWarnings") to soften this warning message to hint.
// You may also define one of the following symbols: NeedMobile, NeedDesktop, NeedMSWindows (or NeedWindows), NeedWin32, NeedWin64, NeedMacOS, NeedMacOS32, NeedMacOS64, NeedIOS, NeedIOS32, NeedIOS64,
// NeedAndroid, NeedLinux, NeedLinux32, NeedPosix, NeedPosix32, NeedCPU32bits (or NeedCPU32), NeedCPU64bits (or NeedCPU64), NeedCPU386, NeedCPUx86, NeedCPUx64, NeedCPUARM, NeedCPUARM32, NeedCPUARM64,
// NeedAssembler, NeedUnicode (or NeedUnicodeStrings), NeedAnsi (or NeedAnsiStrings), NeedOldStrings, NeedNextGen to show fatal message if such feature is not supported.
// You may replace "Need" prefix by "Recommend" (e.g. "RecommendAnsi") to show recommendation warning message (instead of fatal message) if such feature is not supported.
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// p.s. Information is collected from http://docwiki.embarcadero.com/ and https://stackoverflow.com/questions/8460037/list-of-delphi-language-features-and-version-in-which-they-were-introduced-depre //
// Additional information is collected from http://www.interface.ru/home.asp?artId=6763, http://citforum.ru/programming/delphi/delphi2005 and other (marked as "info from another Internet resource"). //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////
// Initial defines //
/////////////////////

{$DEFINE DelphiVerDef7x}     // Indicates usage of this include.
{$DEFINE DelphiVerDef7x100}  // Compatibility version or this include (1.00).
// You can add version check after including this file:
// {$IFNDEF DelphiVerDef7x100}{$MESSAGE FATAL 'DelphiVerDef7x.inc file is not included or incompatible version !!!'}{$ENDIF}

//////////////////////////////////
// Version and features defines //
//////////////////////////////////

{$IFDEF CONDITIONALEXPRESSIONS}

{$IF Defined(IOS) or Defined(ANDROID)}{$DEFINE MOBILE}{$DEFINE NoDESKTOP}{$ELSE}{$DEFINE DESKTOP}{$DEFINE NoMOBILE}{$IFEND}  // Mobile or desktop.

{$IFDEF IOS}{$IFDEF CPU64BITS}{$DEFINE IOS64}{$ELSE}{$DEFINE IOS32}{$ENDIF}{$ENDIF}  // iOS 32 or 64-bit.
{$IFNDEF IOS64}{$DEFINE NoIOS64}{$ENDIF}{$IFNDEF IOS32}{$DEFINE NoIOS32}{$ENDIF}

{$IFNDEF UNICODE}{$DEFINE ANSI}{$DEFINE NoUNICODE}{$ELSE}{$DEFINE NoANSI}{$ENDIF}  // Unicode or ANSI.

{$IF Defined(VER160) or Defined(VER190)}  // Delphi 8 and 2007 for .NET.
  {$MESSAGE FATAL 'This code cannot be compiled by Delphi 8 and 2007 for .NET compilers !!!'}
{$IFEND}

// Delphi 10.1 Berlin
{$IFDEF VER310}{$DEFINE IsDelphiBerlin}{$DEFINE IsDelphi101}{$ENDIF}
{$IF CompilerVersion >= 31}{$DEFINE DelphiBerlin}{$DEFINE Delphi101}
  {$DEFINE Android601}  // Support for Android up to 6.01.
  {$DEFINE UnsafeForAll}{$DEFINE VolForAll}{$DEFINE VolatileForAll}{$DEFINE WeakForAll}{$DEFINE UnsafeVolWeakForAll}  // Now all Delphi compilers supports "unsafe", "volatile" and "weak" attributes.
  {$DEFINE MobUtf8String}{$DEFINE MobRawByteString}  // The string types UTF8String and RawByteString are now supported on iOS and Android.
  {$IFDEF MACOS}{$DEFINE Extended16}{$ENDIF}  // The size of extended on OS X is now 16 bytes.
  {$DEFINE HelpNoPvtAcc}{$DEFINE HelpersNoPrivateAccess}  // To enforce visibility semantics, class and record helpers cannot access private members of the classes or records that they extend.
{$ELSE}{$DEFINE NoDelphiBerlin}{$DEFINE NoDelphi101}
  {$DEFINE NoAndroid601}
  {$DEFINE NoUnsafeForAll}{$DEFINE NoVolForAll}{$DEFINE NoVolatileForAll}{$DEFINE NoWeakForAll}{$DEFINE NoUnsafeVolWeakForAll}
  {$DEFINE NoMobUtf8String}{$DEFINE NoMobRawByteString}
  {$DEFINE NoHelpNoPvtAcc}{$DEFINE NoHelpersNoPrivateAccess}
{$IFEND}
{$IFNDEF Extended16}{$DEFINE NoExtended16}{$ENDIF}

// Delphi 10 Seattle
{$IFDEF VER300}{$DEFINE IsDelphiSeattle}{$DEFINE IsDelphi10}{$ENDIF}
{$IF CompilerVersion >= 30}{$DEFINE DelphiSeattle}{$DEFINE Delphi10}
  {$DEFINE Android511}{$DEFINE iOS84}  // Support for Android 5.1.1 and iOS 8.4.
  // Improved OSX exception handling.
{$ELSE}{$DEFINE NoDelphiSeattle}{$DEFINE NoDelphi10}
  {$DEFINE NoAndroid511}{$DEFINE NoiOS84}
{$IFEND}

// Delphi XE8
{$IFDEF VER290}{$DEFINE IsDelphiXE8}{$ENDIF}
{$IF CompilerVersion >= 29}{$DEFINE DelphiXE8}
  // Support for 64-bit iOS (use "IOS64" symbol defined in this include or build-in "IOS" and "CPU64BITS").
  {$DEFINE FixedIntTypes}  // FinedInt, FixedUInt types support.
  {$IFDEF IOS64}{$DEFINE DblSizeLongTypes}{$ENDIF}  // Double size of LongWord and LongInt (64 bits) for 64-bit iOS platforms.
  {$DEFINE UnitScopedFQN}{$DEFINE UnitScopedFullyQualNames}  // Fully qualified names must now include the unit scope name.
{$ELSE}{$DEFINE NoDelphiXE8}
  {$DEFINE NoFixedIntTypes}
  {$DEFINE NoUnitScopedFQN}{$DEFINE NoUnitScopedFullyQualNames}
{$IFEND}
{$IFNDEF DblSizeLongTypes}{$DEFINE NoDblSizeLongTypes}{$ENDIF}

// Delphi XE7
{$IFDEF VER280}{$DEFINE IsDelphiXE7}{$ENDIF}
{$IF CompilerVersion >= 28}{$DEFINE DelphiXE7}
  {$DEFINE NewDynArrayOps}  // Initialization can be done from declaration, assignation can be done using constant array, additions for arrays, Insert and Delete can specify position, Concat can work with arrays.
  {$DEFINE ParallelLib}  // Parallel Library added to the RTL.
  {$DEFINE NewCompIntr}{$DEFINE NewCompilerIntrin}  // New compiler intrinsic routines (undocumented): IsManagedType, HasWeakRef, GetTypeKind, IsConstValue.
{$ELSE}{$DEFINE NoDelphiXE7}
  {$DEFINE NoNewDynArrayOps}
  {$DEFINE NoParallelLib}
  {$DEFINE NoNewCompIntr}{$DEFINE NoNewCompilerIntrin}
{$IFEND}

// Delphi XE6
{$IFDEF VER270}{$DEFINE IsDelphiXE6}{$ENDIF}
{$IF CompilerVersion >= 27}{$DEFINE DelphiXE6}
  {$DEFINE NewMobHPPEMIT}  // You can now use {$HPPEMIT} directives for mobile apps for linking and generating C++ namespace declarations: {$HPPEMIT NOUSINGNAMESPACE}, {$HPPEMIT LINKUNIT}.
{$ELSE}{$DEFINE NoDelphiXE6}
  {$DEFINE NoNewMobHPPEMIT}
{$IFEND}

// Delphi XE5
{$IFDEF VER260}{$DEFINE IsDelphiXE5}{$ENDIF}
{$IF CompilerVersion >= 26}{$DEFINE DelphiXE5}
  // Android Support; needs device with ArmV6 + Neon or ArmV7 for deployment introduces conditional define ANDROID.
  {$IFDEF NEXTGEN}{$DEFINE ClassOpOverl}{$DEFINE ClassOperatorOverload}{$ENDIF}  // Operator overloading for classes (but only for the NextGen compiler: Android/iOS).
{$ELSE}{$DEFINE NoDelphiXE5}{$IFEND}
{$IFNDEF ClassOpOverl}{$DEFINE NoClassOpOverl}{$DEFINE NoClassOperatorOverload}{$ENDIF}

// Delphi XE4
{$IFDEF VER250}{$DEFINE IsDelphiXE4}{$ENDIF}
{$IF CompilerVersion >= 25}{$DEFINE DelphiXE4}
  // The following new conditionals are introduced/enabled in XE4: AUTOREFCOUNT, CPUARM, EXTERNAL_LINKER, IOS, NEXTGEN, UNDERSCOREIMPORTNAME, WEAKREF, WEAKINSTREF, WEAKINTREF.
  // Reintroduced support for iOS.
  // Before the XE4 release, $IF statements could only be terminated with $IFEND, and the $IFDEF, $IFNDEF, $IFOPT directives could only be terminated with $ENDIF. At XE4, this changed so that $ENDIF became an accepted terminator for $IF, $IFDEF, $IFNDEF, and $IFOPT.
  {$DEFINE Volatile}{$DEFINE Weak}{$DEFINE Ref}  // Volatile, Weak and Ref attributes support (you can use also build-in "WEAKREF" symbol).
  {$IFDEF IOS}{$DEFINE NoOldStrings}{$ENDIF}  // AnsiString, AnsiChar, PAnsiChar, WideString, PWideChar, OpenString, ShortString are not supported in Delphi XE4 or above for iOS.
  {$IFDEF NEXTGEN}{$DEFINE ARCModel}{$ENDIF}  // Automatic Reference Counting: iOS compiler has a brand new memory management model for Delphi.
  {$DEFINE ZeroBasedStrings}  // {$ZEROBASEDSTRINGS ON/OFF} directive support.
  {$DEFINE ExtCompat}{$DEFINE ExtendedCompatibility}  // {$EXTENDEDCOMPATIBILITY ON/OFF} directive support.
{$ELSE}{$DEFINE NoDelphiXE4}
  {$DEFINE NoVolatile}{$DEFINE NoWeak}{$DEFINE NoRef}
  {$DEFINE NoZeroBasedStrings}
  {$DEFINE NoExtCompat}{$DEFINE NoExtendedCompatibility}
{$IFEND}
{$IFNDEF NoOldStrings}{$DEFINE OldStrings}{$ENDIF}
{$IFNDEF ARCModel}{$DEFINE NoARCModel}{$ENDIF}

// Delphi XE3
{$IFDEF VER240}{$DEFINE IsDelphiXE3}{$ENDIF}
{$IF CompilerVersion >= 24}{$DEFINE DelphiXE3}
  // Removed support for iOS.
  {$DEFINE RecHelpers}  // Record helpers became more powerful by allowing to extend simple Delphi types like strings, integers, enums, sets and alike). The System.SysUtils unit implements a record named "TStringHelper" which is actually a record helper for strings.
  {$DEFINE AtomicIntrin}  // Atomic intrinsic functions: AtomicExchange, AtomicIncrement, AtomicCmpExchange, AtomicDecrement.
{$ELSE}{$DEFINE NoDelphiXE3}
  {$DEFINE NoRecHelpers}
  {$DEFINE NoAtomicIntrin}
{$IFEND}

// Delphi XE2
{$IFDEF VER230}{$DEFINE IsDelphiXE2}{$ENDIF}
{$IF CompilerVersion >= 23}{$DEFINE DelphiXE2}
  // Cross platform support for Mac OSX (32-bit) and iOS; Support for Win64.
  // New build-in defines: ALIGN_STACK, CPUX86, CPUX64, MACOS, MACOS32, PC_MAPPED_EXCEPTIONS, PIC, WIN64.
  {$DEFINE UnitScopeNames}  // Full unit scope names are now required in your "uses" clause (Winapi.Windows instead of Windows, System.SysUtils instead of SysUtils, etc).
  {$DEFINE CrossPlatformRTL}  // Modified RTL to support cross platform.
  {$DEFINE PackedByteAlign}  // Keyword "packed" now forces byte alignment of records (pre XE2 it did not necessarily do this).
  {$IFDEF WIN64}{$DEFINE Extended8}{$ENDIF}  // Extended Data Type Is 10 bytes on Win32, but 8 (!) bytes on Win64.
{$ELSE}{$DEFINE NoDelphiXE2}
  {$DEFINE NoUnitScopeNames}
  {$DEFINE NoCrossPlatformRTL}
  {$DEFINE NoPackedByteAlign}
{$IFEND}
{$IFNDEF Extended8}{$DEFINE NoExtended8}{$ENDIF}

// Delphi XE
{$IFDEF VER220}{$DEFINE IsDelphiXE}{$DEFINE IsDelphiXE1}{$ENDIF}
{$IF CompilerVersion >= 22}{$DEFINE DelphiXE}{$DEFINE DelphiXE1}
  {$DEFINE Align16}  // New 16-byte value for the {$ALIGN} directive: {$A16}.
  {$DEFINE CodeAlign}  // New {$CODEALIGN} directive, this sets the starting address for a procedure or function.
  {$DEFINE StrongLinkTypes}  // The {$STRONGLINKTYPES ON} directive support.
  {$DEFINE RegExp}  // Support for regular expressions.
  {$DEFINE IngoreStringChecks}  // The {$STRINGCHECKS} compiler directive is ignored in XE.
{$ELSE}{$DEFINE NoDelphiXE}{$DEFINE NoDelphiXE1}
  {$DEFINE NoAlign16}
  {$DEFINE NoCodeAlign}
  {$DEFINE NoStrongLinkTypes}
  {$DEFINE NoRegExp}
  {$DEFINE NoIngoreStringChecks}
{$IFEND}

// Delphi 2010
{$IFDEF VER210}{$DEFINE IsDelphi2010}{$ENDIF}
{$IF CompilerVersion >= 21}{$DEFINE Delphi2010}
  {$DEFINE EnhancedRTTI}  // Enhanced Delphi RTTI (Run Time Type Information).
  {$DEFINE Attributes}  // Attributes and TCustomAttribute support. Attributes are a way of associating additional metadata information with a given type or member of a type. They can be applied in many places, the following code shows several of the places you can place attributes.
  {$DEFINE Delayed}  // New "delayed" directive indicates that an external library such as a DLL is not to be loaded at declaration time but is to wait until the first call to the method.
  {$DEFINE ClassConstr}  // Class Constructors and Destructors.
  {$DEFINE ExtraInterfaceOps}  // The "as" operator can be used to cast an interface reference back to the object from which it was extracted. The "is" operator can be used to verify whether an interface reference was extracted from a certain class. Normal unsafe casting can be performed on an interface: TObject(SomeInterface).
{$ELSE}{$DEFINE NoDelphi2010}
  {$DEFINE NoEnhancedRTTI}
  {$DEFINE NoAttributes}
  {$DEFINE NoDelayed}
  {$DEFINE NoClassConstr}
  {$DEFINE NoExtraInterfaceOps}
{$IFEND}

// Delphi 2009
{$IFDEF VER200}{$DEFINE IsDelphi2009}{$ENDIF}
{$IF CompilerVersion >= 20}{$DEFINE Delphi2009}
  // Intrinsic type string now maps to UnicodeString (use build-in "UNICODE" symbol).
  // "Resourcestrings" as Widestrings.
  {$DEFINE DefaultIntrin}  // New undocumented intrinsic function Default(T: Typeidentifier): value of T.
  {$DEFINE Generics}  // Generics or generic types (also know as parametrized types), allow you to define classes that don't specifically define the type of certain data members.
  {$DEFINE SmartPointers}  // Strongly-typed smart pointers are now possible in Delphi, leveraging the work on generics.
  {$DEFINE AnonymMethods}  // Anonymous method is a procedure or function that does not have a name associated with it.
  {$DEFINE NestedExcepts}  // Support for nested exceptions and exception tracing.
  {$DEFINE PointerMath}  // Support for pointermath and a new compiler directive: {$POINTERMATH ON|OFF}.
  {$DEFINE ExitParam}  // The Exit function can take a parameter specifying a result.
  {$DEFINE TMonitor}  // TObject has a extra hidden pointer to TMonitor in addition to its VMT pointer.
  {$DEFINE DeprecatedText}  // The "deprecated" keyword can now have additional text.
  // Four new compiler warnings: W1057 Implicit string cast from '%s' to '%s', W1058 Implicit string cast with potential data loss from '%s' to '%s', W1059 Explicit string cast from '%s' to '%s', W1060 Explicit string cast with potential data loss from '%s' to '%s'.
{$ELSE}{$DEFINE NoDelphi2009}
  {$DEFINE NoDefaultIntrin}
  {$DEFINE NoGenerics}
  {$DEFINE NoSmartPointers}
  {$DEFINE NoAnonymMethods}
  {$DEFINE NoNestedExcepts}
  {$DEFINE NoPointerMath}
  {$DEFINE NoExitParam}
  {$DEFINE NoTMonitor}
  {$DEFINE NoDeprecatedText}
{$IFEND}

// Delphi 2007
{$IFDEF VER185}{$DEFINE IsDelphi2007}{$ENDIF}
{$IF CompilerVersion >= 18.5}{$DEFINE Delphi2007}
  // No language changes that I know of. Note that Delphi 2007 is a non-breaking release, DCU's from Delphi 2006 will work unchanged in Delphi 2007.
{$ELSE}{$DEFINE NoDelphi2007}{$IFEND}

// Delphi 2006
{$IF Defined(VER180) and not Defined(VER185)}{$DEFINE IsDelphi2006}{$IFEND}
{$IF CompilerVersion >= 18}{$DEFINE Delphi2006}
  {$DEFINE RecMethods}  // Enhanced records (records with methods).
  {$DEFINE OpOverl}{$DEFINE OperatorOverload}  // Operator overloading.
  {$DEFINE ClassVars}  // Added "class var" support [info from another Internet resource].
  {$DEFINE FastMM}  // FastMM is the default memory manager.
{$ELSE}{$DEFINE NoDelphi2006}
  {$DEFINE NoRecMethods}
  {$DEFINE NoOpOverl}{$DEFINE NoOperatorOverload}
  {$DEFINE NoFastMM}
  {$DEFINE NoClassVars}
{$IFEND}

// Delphi 2005
{$IFDEF VER170}{$DEFINE IsDelphi2005}{$ENDIF}
{$IF CompilerVersion >= 17}{$DEFINE Delphi2005}
  {$DEFINE ForIn}  // For ... in loops.
  {$DEFINE Inline}  // Added "inline" directive support.
  {$DEFINE DynArrayInit}  // Dynamic arrays initialization: Arr := TArr.Create(1, 2, 3) [info from another Internet resource].
  {$DEFINE NestedTypes}  // Nested types [and classes - info from another Internet resource].
  {$DEFINE NestedConst}  // Nested constants [and vars - info from another Internet resource].
  {$DEFINE ClassProperty}  // Added "class property" support [info from another Internet resource].
  {$DEFINE Unsafe}  // Added "unsafe" keyword [info from another Internet resource].
  {$DEFINE UnicodeIds}  // Added unicode identifiers support (non-Latin letters in names) [info from another Internet resource].
  {$DEFINE UsesWildcards}  // Wildcard in uses statement allowed.
  {$DEFINE Region}  // Added {$REGION}/{$ENDREGION} directives.
  // Thw following features described (on "stackoverflow.com" forum topic, see above the link) as new features of Delphi 2006 but practically they work in Delphi 2005 too.
  {$DEFINE ClassHelpers}  // Class helpers.
  {$DEFINE Static}  // Added "static" methods and properties.
  {$DEFINE Strict}  // Added "strict" private/protected visibility keyword.
  {$DEFINE Sealed}  // Added "sealed" keyword for calsses [info from another Internet resource].
  {$DEFINE Final}  // Added "final" keyword for virtual methods.
  {$DEFINE AbsClass}{$DEFINE AbstractClasses}  // Added "abstract" keyword for calsses [info from another Internet resource].
  {$DEFINE Experimental}  // New "experimental" keyword [info from another Internet resource].
  {$DEFINE ArrPropOverl}{$DEFINE ArrayPropertyOverload}  // Default array properties overload [info from another Internet resource].
  {$DEFINE MethodInfo}  // Added {$METHODINFO} directive.
{$ELSE}{$DEFINE NoDelphi2005}
  {$DEFINE NoForIn}
  {$DEFINE NoInline}
  {$DEFINE NoDynArrayInit}
  {$DEFINE NoNestedTypes}
  {$DEFINE NoNestedConst}
  {$DEFINE NoClassProperty}
  {$DEFINE NoUnsafe}
  {$DEFINE NoUnicodeIds}
  {$DEFINE NoUsesWildcards}
  {$DEFINE NoRegion}
  {$DEFINE NoStatic}
  {$DEFINE NoClassHelpers}
  {$DEFINE NoStrict}
  {$DEFINE NoSealed}
  {$DEFINE NoFinal}
  {$DEFINE NoAbsClass}{$DEFINE NoAbstractClasses}
  {$DEFINE NoExperimental}
  {$DEFINE NoArrPropOverl}{$DEFINE NoArrayPropertyOverload}
  {$DEFINE NoMethodInfo}
{$IFEND}

// Delphi 7
{$IFDEF VER150}{$DEFINE IsDelphi7}{$ENDIF}
{$IF CompilerVersion >= 15}{$DEFINE Delphi7}
  // Three additional compiler warnings: Unsafe_Type, Unsafe_Code and Unsafe_Cast. These warnings are disabled by default, but can be enabled.
  {$DEFINE WarnUnsafeCode}  // New compiler directive {$WARN UNSAFE_CODE ON}.
  {$DEFINE TFmtSetOverl}{$DEFINE TFormatSettingsOverloads}  // Overloads of routines that format and parse numbers, date-time values, and currency using a TFormatSettings structure.
{$ELSE}{$DEFINE NoDelphi7}
  {$DEFINE NoWarnUnsafeCode}
  {$DEFINE NoTFmtSetOverl}{$DEFINE NoTFormatSettingsOverloads}
{$IFEND}

// Delphi 6
{$IFDEF VER140}{$DEFINE IsDelphi6}{$ENDIF}
{$IF CompilerVersion >= 14}{$DEFINE Delphi6}
{$ELSE}{$DEFINE NoDelphi6}{$IFEND}  // To be on the safe side :)

//////////////////////////////////
// Compiler compatibility check //
//////////////////////////////////

{$IF CompilerVersion > 31}
  {$IF Defined(NotAboveDelphiBerlin) or Defined(NotAboveDelphi101)}{$MESSAGE FATAL 'You need Delphi 10.1 Berlin or below to compile this !!!'}{$IFEND}
{$IFEND}

// Delphi 10.1 Berlin
{$IFNDEF DelphiBerlin}
  {$IF Defined(NeedDelphiBerlin) or Defined(NeedDelphi101)}{$MESSAGE FATAL 'You need at least Delphi 10.1 Berlin or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiSeattle) or Defined(NotAboveDelphi10)}{$MESSAGE FATAL 'You need Delphi 10 Seattle or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiBerlin}
  {$IF Defined(NotForDelphiBerlin) or Defined(NotForDelphi101)}{$MESSAGE FATAL 'You can''t use Delphi 10.1 Berlin to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiBerlin) or Defined(TestedDelphi101)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 10.1 Berlin compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiBerlin) or Defined(OnlyDelphi101)}{$MESSAGE FATAL 'You need exactly Delphi 10.1 Berlin to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi 10 Seattle
{$IFNDEF DelphiSeattle}
  {$IF Defined(NeedDelphiSeattle) or Defined(NeedDelphi10)}{$MESSAGE FATAL 'You need at least Delphi 10 Seattle or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiXE8)}{$MESSAGE FATAL 'You need Delphi XE8 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiSeattle}
  {$IF Defined(NotForDelphiSeattle) or Defined(NotForDelphi10)}{$MESSAGE FATAL 'You can''t use Delphi 10 Seattle to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiSeattle) or Defined(TestedDelphi10)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 10 Seattle compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiSeattle) or Defined(OnlyDelphi10)}{$MESSAGE FATAL 'You need exactly Delphi 10 Seattle to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi XE8
{$IFNDEF DelphiXE8}
  {$IF Defined(NeedDelphiXE8)}{$MESSAGE FATAL 'You need at least Delphi XE8 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiXE7)}{$MESSAGE FATAL 'You need Delphi XE7 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiXE8}
  {$IF Defined(NotForDelphiXE8)}{$MESSAGE FATAL 'You can''t use Delphi XE8 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiXE8)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi XE8 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiXE8)}{$MESSAGE FATAL 'You need exactly Delphi XE8 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi XE7
{$IFNDEF DelphiXE7}
  {$IF Defined(NeedDelphiXE7)}{$MESSAGE FATAL 'You need at least Delphi XE7 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiXE6)}{$MESSAGE FATAL 'You need Delphi XE6 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiXE7}
  {$IF Defined(NotForDelphiXE7)}{$MESSAGE FATAL 'You can''t use Delphi XE7 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiXE7)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi XE7 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiXE7)}{$MESSAGE FATAL 'You need exactly Delphi XE7 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi XE6
{$IFNDEF DelphiXE6}
  {$IF Defined(NeedDelphiXE6)}{$MESSAGE FATAL 'You need at least Delphi XE6 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiXE5)}{$MESSAGE FATAL 'You need Delphi XE5 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiXE6}
  {$IF Defined(NotForDelphiXE6)}{$MESSAGE FATAL 'You can''t use Delphi XE6 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiXE6)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi XE6 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiXE6)}{$MESSAGE FATAL 'You need exactly Delphi XE6 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi XE5
{$IFNDEF DelphiXE5}
  {$IF Defined(NeedDelphiXE5)}{$MESSAGE FATAL 'You need at least Delphi XE5 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiXE4)}{$MESSAGE FATAL 'You need Delphi XE4 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiXE5}
  {$IF Defined(NotForDelphiXE5)}{$MESSAGE FATAL 'You can''t use Delphi XE5 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiXE5)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi XE5 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiXE5)}{$MESSAGE FATAL 'You need exactly Delphi XE5 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi XE4
{$IFNDEF DelphiXE4}
  {$IF Defined(NeedDelphiXE4)}{$MESSAGE FATAL 'You need at least Delphi XE4 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiXE3)}{$MESSAGE FATAL 'You need Delphi XE3 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiXE4}
  {$IF Defined(NotForDelphiXE4)}{$MESSAGE FATAL 'You can''t use Delphi XE4 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiXE4)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi XE4 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiXE4)}{$MESSAGE FATAL 'You need exactly Delphi XE4 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi XE3
{$IFNDEF DelphiXE3}
  {$IF Defined(NeedDelphiXE3)}{$MESSAGE FATAL 'You need at least Delphi XE3 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiXE2)}{$MESSAGE FATAL 'You need Delphi XE2 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiXE3}
  {$IF Defined(NotForDelphiXE3)}{$MESSAGE FATAL 'You can''t use Delphi XE3 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiXE3)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi XE3 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiXE3)}{$MESSAGE FATAL 'You need exactly Delphi XE3 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi XE2
{$IFNDEF DelphiXE2}
  {$IF Defined(NeedDelphiXE2)}{$MESSAGE FATAL 'You need at least Delphi XE2 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphiXE) or Defined(NotAboveDelphiXE1)}{$MESSAGE FATAL 'You need Delphi XE or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiXE2}
  {$IF Defined(NotForDelphiXE2)}{$MESSAGE FATAL 'You can''t use Delphi XE2 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiXE2)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi XE2 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiXE2)}{$MESSAGE FATAL 'You need exactly Delphi XE2 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi XE
{$IFNDEF DelphiXE}
  {$IF Defined(NeedDelphiXE) or Defined(NeedDelphiXE1)}{$MESSAGE FATAL 'You need at least Delphi XE or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphi2010)}{$MESSAGE FATAL 'You need Delphi 2010 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphiXE}
  {$IF Defined(NotForDelphiXE) or Defined(NotForDelphiXE1)}{$MESSAGE FATAL 'You can''t use Delphi XE to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphiXE) or Defined(TestedDelphiXE1)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi XE compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphiXE) or Defined(OnlyDelphiXE1)}{$MESSAGE FATAL 'You need exactly Delphi XE to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi 2010
{$IFNDEF Delphi2010}
  {$IF Defined(NeedDelphi2010)}{$MESSAGE FATAL 'You need at least Delphi 2010 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphi2009)}{$MESSAGE FATAL 'You need Delphi 2009 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphi2010}
  {$IF Defined(NotForDelphi2010)}{$MESSAGE FATAL 'You can''t use Delphi 2010 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphi2010)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 2010 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphi2010)}{$MESSAGE FATAL 'You need exactly Delphi 2010 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi 2009
{$IFNDEF Delphi2009}
  {$IF Defined(NeedDelphi2009)}{$MESSAGE FATAL 'You need at least Delphi 2009 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphi2007)}{$MESSAGE FATAL 'You need Delphi 2007 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphi2009}
  {$IF Defined(NotForDelphi2009)}{$MESSAGE FATAL 'You can''t use Delphi 2009 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphi2009)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 2009 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphi2009)}{$MESSAGE FATAL 'You need exactly Delphi 2009 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi 2007
{$IFNDEF Delphi2007}
  {$IF Defined(NeedDelphi2007)}{$MESSAGE FATAL 'You need at least Delphi 2007 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphi2006)}{$MESSAGE FATAL 'You need Delphi 2006 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphi2007}
  {$IF Defined(NotForDelphi2007)}{$MESSAGE FATAL 'You can''t use Delphi 2007 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphi2007)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 2007 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphi2007)}{$MESSAGE FATAL 'You need exactly Delphi 2007 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi 2006
{$IFNDEF Delphi2006}
  {$IF Defined(NeedDelphi2006)}{$MESSAGE FATAL 'You need at least Delphi 2006 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphi2005)}{$MESSAGE FATAL 'You need Delphi 2005 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphi2006}
  {$IF Defined(NotForDelphi2006)}{$MESSAGE FATAL 'You can''t use Delphi 2006 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphi2006)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 2006 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphi2006)}{$MESSAGE FATAL 'You need exactly Delphi 2006 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi 2005
{$IFNDEF Delphi2005}
  {$IF Defined(NeedDelphi2005)}{$MESSAGE FATAL 'You need at least Delphi 2005 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphi7)}{$MESSAGE FATAL 'You need Delphi 7 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphi2005}
  {$IF Defined(NotForDelphi2005)}{$MESSAGE FATAL 'You can''t use Delphi 2005 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphi2005)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 2005 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphi2005)}{$MESSAGE FATAL 'You need exactly Delphi 2005 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi 7
{$IFNDEF Delphi7}
  {$IF Defined(NeedDelphi7)}{$MESSAGE FATAL 'You need at least Delphi 7 or above to compile this !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(NotAboveDelphi6)}{$MESSAGE FATAL 'You need Delphi 6 or below to compile this !!!'}{$IFEND}
{$ENDIF}
{$IFDEF IsDelphi7}
  {$IF Defined(NotForDelphi7)}{$MESSAGE FATAL 'You can''t use Delphi 7 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphi7)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 7 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphi7)}{$MESSAGE FATAL 'You need exactly Delphi 7 to compile this !!!'}{$IFEND}
{$ENDIF}

// Delphi 6
{$IFDEF IsDelphi6}
  {$IF Defined(NotForDelphi6)}{$MESSAGE FATAL 'You can''t use Delphi 6 to compile this !!!'}{$IFEND}
  {$IF Defined(TestedDelphi6)}{$DEFINE TestedThisDelphi}{$IFEND}
  {$IF Defined(TestWarnings) and not Defined(TestedThisDelphi)}{$MESSAGE WARN 'This code is not tested with Delphi 6 compiler !!!'}{$IFEND}
{$ELSE}
  {$IF Defined(OnlyDelphi7)}{$MESSAGE FATAL 'You need exactly Delphi 6 to compile this !!!'}{$IFEND}
{$ENDIF}

//////////////////////////////////
// Best designed compiler check //
//////////////////////////////////

// Delphi 10.1 Berlin
{$IF (Defined(DesignedForDelphiBerlin) or Defined(DesignedForDelphi101)) and not Defined(IsDelphiBerlin)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 10.1 Berlin compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 10.1 Berlin compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi 10 Seattle
{$IF (Defined(DesignedForDelphiSeattle) or Defined(DesignedForDelphi10)) and not Defined(IsDelphiSeattle)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 10 Seattle compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 10 Seattle compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi XE8
{$IF Defined(DesignedForDelphiXE8) and not Defined(IsDelphiXE8)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi XE8 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi XE8 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi XE7
{$IF Defined(DesignedForDelphiXE7) and not Defined(IsDelphiXE7)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi XE7 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi XE7 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi XE6
{$IF Defined(DesignedForDelphiXE6) and not Defined(IsDelphiXE6)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi XE6 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi XE6 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi XE5
{$IF Defined(DesignedForDelphiXE5) and not Defined(IsDelphiXE5)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi XE5 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi XE5 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi XE4
{$IF Defined(DesignedForDelphiXE4) and not Defined(IsDelphiXE4)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi XE4 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi XE4 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi XE3
{$IF Defined(DesignedForDelphiXE3) and not Defined(IsDelphiXE3)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi XE3 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi XE3 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi XE2
{$IF Defined(DesignedForDelphiXE2) and not Defined(IsDelphiXE2)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi XE2 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi XE2 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi XE
{$IF (Defined(DesignedForDelphiXE) or Defined(DesignedForDelphiXE1)) and not Defined(IsDelphiXE)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi XE compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi XE compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi 2010
{$IF Defined(DesignedForDelphi2010) and not Defined(IsDelphi2010)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 2010 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 2010 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi 2009
{$IF Defined(DesignedForDelphi2009) and not Defined(IsDelphi2009)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 2009 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 2009 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi 2007
{$IF Defined(DesignedForDelphi2007) and not Defined(IsDelphi2007)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 2007 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 2007 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi 2006
{$IF Defined(DesignedForDelphi2006) and not Defined(IsDelphi2006)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 2006 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 2006 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi 2005
{$IF Defined(DesignedForDelphi2005) and not Defined(IsDelphi2005)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 2005 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 2005 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi 7
{$IF Defined(DesignedForDelphi7) and not Defined(IsDelphi7)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 7 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 7 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

// Delphi 6
{$IF Defined(DesignedForDelphi6) and not Defined(IsDelphi6)}
  {$IFNDEF TestedThisDelphi}
    {$MESSAGE WARN 'This code is best designed for Delphi 6 compiler !!!'}
  {$ELSE}
    {$MESSAGE HINT 'This code is best designed for Delphi 6 compiler but also was well-tested with your compiler version too!'}
  {$ENDIF}
{$IFEND}

/////////////////////////////
// Required features check //
/////////////////////////////

{$IF Defined(NeedMobile) and not Defined (MOBILE)}{$MESSAGE FATAL 'This code can be compiled for Mobile only !!!'}{$IFEND}
{$IF Defined(NeedDesktop) and not Defined (DESKTOP)}{$MESSAGE FATAL 'This code can be compiled for Desktop only !!!'}{$IFEND}
{$IF (Defined(NeedMSWindows) or Defined(NeedWindows)) and not Defined (MSWINDOWS)}{$MESSAGE FATAL 'This code can be compiled for Microsoft Windows only !!!'}{$IFEND}
{$IF Defined(NeedWin32) and not Defined (WIN32)}{$MESSAGE FATAL 'This code can be compiled for 32-bit Microsoft Windows only !!!'}{$IFEND}
{$IF Defined(NeedWin64) and not Defined (WIN64)}{$MESSAGE FATAL 'This code can be compiled for 64-bit Microsoft Windows only !!!'}{$IFEND}
{$IF Defined(NeedMacOS) and not Defined (MACOS)}{$MESSAGE FATAL 'This code can be compiled for Mac OS X only !!!'}{$IFEND}
{$IF Defined(NeedMacOS32) and not Defined (MACOS32)}{$MESSAGE FATAL 'This code can be compiled for 32-bit Mac OS X only !!!'}{$IFEND}
{$IF Defined(NeedMacOS64) and not Defined (MACOS64)}{$MESSAGE FATAL 'This code can be compiled for 64-bit Mac OS X only !!!'}{$IFEND}
{$IF Defined(NeedIOS) and not Defined (IOS)}{$MESSAGE FATAL 'This code can be compiled for iOS only !!!'}{$IFEND}
{$IF Defined(NeedIOS32) and not Defined (IOS32)}{$MESSAGE FATAL 'This code can be compiled for 32-bit iOS only !!!'}{$IFEND}
{$IF Defined(NeedIOS64) and not Defined (IOS64)}{$MESSAGE FATAL 'This code can be compiled for 64-bit iOS only !!!'}{$IFEND}
{$IF Defined(NeedAndroid) and not Defined (ANDROID)}{$MESSAGE FATAL 'This code can be compiled for Android only !!!'}{$IFEND}
{$IF Defined(NeedLinux) and not Defined (LINUX)}{$MESSAGE FATAL 'This code can be compiled for Linux only !!!'}{$IFEND}
{$IF Defined(NeedLinux32) and not Defined (LINUX32)}{$MESSAGE FATAL 'This code can be compiled for 32-bit Linux only !!!'}{$IFEND}
{$IF Defined(NeedPosix) and not Defined (POSIX)}{$MESSAGE FATAL 'This code can be compiled for Posix only !!!'}{$IFEND}
{$IF Defined(NeedPosix32) and not Defined (POSIX32)}{$MESSAGE FATAL 'This code can be compiled for 32-bit Posix only !!!'}{$IFEND}
{$IF (Defined(NeedCPU32bits) or Defined(NeedCPU32)) and not Defined (CPU32BITS)}{$MESSAGE FATAL 'This code can be compiled for 32-bit CPU only !!!'}{$IFEND}
{$IF (Defined(NeedCPU64bits) or Defined(NeedCPU64)) and not Defined (CPU64BITS)}{$MESSAGE FATAL 'This code can be compiled for 64-bit CPU only !!!'}{$IFEND}
{$IF Defined(NeedCPU386) and not Defined (CPU386)}{$MESSAGE FATAL 'This code can be compiled for Intel 386+ CPU only !!!'}{$IFEND}
{$IF Defined(NeedCPUx86) and not Defined (CPUX86)}{$MESSAGE FATAL 'This code can be compiled for 32-bit Intel CPU only !!!'}{$IFEND}
{$IF Defined(NeedCPUx64) and not Defined (CPUX64)}{$MESSAGE FATAL 'This code can be compiled for 64-bit Intel CPU only !!!'}{$IFEND}
{$IF Defined(NeedCPUARM) and not Defined (CPUARM)}{$MESSAGE FATAL 'This code can be compiled for CPU based on the ARM architecture only !!!'}{$IFEND}
{$IF Defined(NeedCPUARM32) and not Defined (CPUARM32)}{$MESSAGE FATAL 'This code can be compiled for 32-bit CPU based on the ARM architecture only !!!'}{$IFEND}
{$IF Defined(NeedCPUARM64) and not Defined (CPUARM64)}{$MESSAGE FATAL 'This code can be compiled for 64-bit CPU based on the ARM architecture only !!!'}{$IFEND}
{$IF Defined(NeedAssembler) and not Defined (ASSEMBLER)}{$MESSAGE FATAL 'This code requires Assembler support to be compiled !!!'}{$IFEND}
{$IF (Defined(NeedUnicode) or Defined(NeedUnicodeStrings)) and not Defined (UNICODE)}{$MESSAGE FATAL 'This code requires Unicode Strings defaults (Delphi 2009 or above) to be compiled !!!'}{$IFEND}
{$IF (Defined(NeedAnsi) or Defined(NeedAnsiStrings)) and Defined (UNICODE)}{$MESSAGE FATAL 'This code requires ANSI Strings defaults (Delphi 2007 or below) to be compiled !!!'}{$IFEND}
{$IF Defined(NeedOldStrings) and not Defined (OldStrings)}{$MESSAGE FATAL 'This code requires Old Strings Types support (Delphi XE3 or below for iOS) to be compiled !!!'}{$IFEND}
{$IF Defined(NeedNextGen) and Defined (NEXTGEN)}{$MESSAGE FATAL 'This code requires "Next-Generation" language features (such as the Delphi mobile compiler) to be compiled !!!'}{$IFEND}

////////////////////////////////
// Recommended features check //
////////////////////////////////

{$IF Defined(RecommendMobile) and not Defined (MOBILE)}{$MESSAGE FATAL 'It''s recommended to use Mobile platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendDesktop) and not Defined (DESKTOP)}{$MESSAGE FATAL 'It''s recommended to use Desktop platform compiler for this code !!!'}{$IFEND}
{$IF (Defined(RecommendMSWindows) or Defined(RecommendWindows)) and not Defined (MSWINDOWS)}{$MESSAGE FATAL 'It''s recommended to use Microsoft Windows platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendWin32) and not Defined (WIN32)}{$MESSAGE FATAL 'It''s recommended to use 32-bit Microsoft Windows platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendWin64) and not Defined (WIN64)}{$MESSAGE FATAL 'It''s recommended to use 64-bit Microsoft Windows platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendMacOS) and not Defined (MACOS)}{$MESSAGE FATAL 'It''s recommended to use Mac OS X platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendMacOS32) and not Defined (MACOS32)}{$MESSAGE FATAL 'It''s recommended to use 32-bit Mac OS X platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendMacOS64) and not Defined (MACOS64)}{$MESSAGE FATAL 'It''s recommended to use 64-bit Mac OS X platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendIOS) and not Defined (IOS)}{$MESSAGE FATAL 'It''s recommended to use iOS platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendIOS32) and not Defined (IOS32)}{$MESSAGE FATAL 'It''s recommended to use 32-bit iOS platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendIOS64) and not Defined (IOS64)}{$MESSAGE FATAL 'It''s recommended to use 64-bit iOS platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendAndroid) and not Defined (ANDROID)}{$MESSAGE FATAL 'It''s recommended to use Android platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendLinux) and not Defined (LINUX)}{$MESSAGE FATAL 'It''s recommended to use Linux platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendLinux32) and not Defined (LINUX32)}{$MESSAGE FATAL 'It''s recommended to use 32-bit Linux platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendPosix) and not Defined (POSIX)}{$MESSAGE FATAL 'It''s recommended to use Posix platform compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendPosix32) and not Defined (POSIX32)}{$MESSAGE FATAL 'It''s recommended to use 32-bit Posix platform compiler for this code !!!'}{$IFEND}
{$IF (Defined(RecommendCPU32bits) or Defined(RecommendCPU32)) and not Defined (CPU32BITS)}{$MESSAGE FATAL 'It''s recommended to use 32-bit CPU compiler for this code !!!'}{$IFEND}
{$IF (Defined(RecommendCPU64bits) or Defined(RecommendCPU64)) and not Defined (CPU64BITS)}{$MESSAGE FATAL 'It''s recommended to use 64-bit CPU compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendCPU386) and not Defined (CPU386)}{$MESSAGE FATAL 'It''s recommended to use Intel 386+ CPU compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendCPUx86) and not Defined (CPUX86)}{$MESSAGE FATAL 'It''s recommended to use 32-bit Intel CPU compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendCPUx64) and not Defined (CPUX64)}{$MESSAGE FATAL 'It''s recommended to use 64-bit Intel CPU compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendCPUARM) and not Defined (CPUARM)}{$MESSAGE FATAL 'It''s recommended to use ARM architecture based CPU compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendCPUARM32) and not Defined (CPUARM32)}{$MESSAGE FATAL 'It''s recommended to use 32-bit ARM architecture based CPU compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendCPUARM64) and not Defined (CPUARM64)}{$MESSAGE FATAL 'It''s recommended to use 64-bit ARM architecture based CPU compiler for this code !!!'}{$IFEND}
{$IF Defined(RecommendAssembler) and not Defined (ASSEMBLER)}{$MESSAGE FATAL 'It''s recommended to use compiler with Assembler support for this code !!!'}{$IFEND}
{$IF (Defined(RecommendUnicode) or Defined(RecommendUnicodeStrings)) and not Defined (UNICODE)}{$MESSAGE WARN 'It''s recommended to use compiler with Unicode Strings defaults (Delphi 2009 or above) for this code !!!'}{$IFEND}
{$IF (Defined(RecommendAnsi) or Defined(RecommendAnsiStrings)) and Defined (UNICODE)}{$MESSAGE WARN 'It''s recommended to use compiler with ANSI Strings defaults (Delphi 2007 or below) for this code !!!'}{$IFEND}
{$IF Defined(RecommendOldStrings) and not Defined (OldStrings)}{$MESSAGE FATAL 'It''s recommended to use compiler with Old Strings Types support (Delphi XE3 or below for iOS) for this code !!!'}{$IFEND}
{$IF Defined(RecommendNextGen) and Defined (NEXTGEN)}{$MESSAGE FATAL 'It''s recommended to use compiler with "Next-Generation" language features (such as the Delphi mobile compiler) for this code !!!'}{$IFEND}

{$ELSE !CONDITIONALEXPRESSIONS}
  __YOU_NEED_AT_LEAST_DELPHI_6_OR_ABOVE_TO_COMPILE_THIS__
{$ENDIF}
