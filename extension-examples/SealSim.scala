// Warning: the seals may heat up your machine.

// Requires: Std, String, Random.

object SealSim {
    abstract class Gender
    case class Male() extends Gender
    case class Female() extends Gender

    abstract class Mood
    case class Happy() extends Mood
    case class Belligerent() extends Mood
    case class Crazy() extends Mood
    case class Moody() extends Mood
    case class Sad() extends Mood
    case class Angry() extends Mood
    case class Bonkers() extends Mood

    abstract class Colour
    case class RGB(r: Int, g: Int, b: Int) extends Colour

    abstract class Seal
    case class Sealio(name: String, age: Int, g: Gender, m: Mood, c: Colour) extends Seal

    abstract class SealList
    case class Nil() extends SealList
    case class Cons(h: Seal, t: SealList) extends SealList

    def grey(x: Int): Colour = {
        RGB(x, x, x)
    }

    def gender(s: Seal): Gender = {
        s match {
            case Sealio(_, _, g, _, _) => g
        }
    }

    def length(sl: SealList): Int = {
        sl match {
            case Nil() => 0
            case Cons(_, xs) => 1 + length(xs)
        }
    }

    def head(sl: SealList): Seal = {
        sl match {
            case Nil() => error("empty seal list!")
            case Cons(h, _) => h
        }
    }

    def isEmpty(sl: SealList): Boolean = {
        sl match {
            case Nil() => true
            case Cons(_, _) => false
        }
    }

    def breed(a: Seal, b: Seal): Seal = {
        (a, b) match {
            case (Sealio(nameA, _, genderA, _, colourA),
                  Sealio(nameB, _, genderB, _, colourB)) =>
                log(nameA ++ " is mating with " ++ nameB ++ "!");
                val s: (String, Gender) = randomSeal();
                val m: Mood = randomMood();
                val c: Colour = mixColour(colourA, colourB, 30);
                log(s(0) ++ " was born!");
                Sealio(s(0), 0, s(1), m, c)
        }
    }

    // Returns a tuple of (first_candidate_opt, the_rest).
    def findPartner(pop: SealList, seeker: Seal): (SealList, SealList) = {
        val gSeeker: Gender = gender(seeker);
        pop match {
            case Nil() => (Nil(), Nil())
            case Cons(x, xs) =>
                x match {
                    case Sealio(_, _, g, _, _) =>
                        if (!isGenderEqual(g, gSeeker)) {
                            (Cons(x, Nil()), xs)
                        } else {
                            val rec: (SealList, SealList) = findPartner(xs, seeker);
                            (rec(0), Cons(x, rec(1)))
                        }
                }
        }
    }

    def simulatei(pop: SealList, cubs: SealList): SealList = {
        pop match {
            case Nil() => cubs
            case Cons(x, xs) =>
                // Cons(x, cubs)
                val res: (SealList, SealList) = findPartner(xs, x);
                val hasPartner: Boolean = !isEmpty(res(0));
                if (hasPartner) {
                    val partner: Seal = head(res(0));
                    val cub: Seal = breed(x, partner);
                    killOrStep(x, killOrStep(partner, simulatei(res(1), Cons(cub, cubs))))
                } else {
                    killOrStep(x, simulatei(xs, cubs))
                }
        }
    }
    
    def simulate(pop: SealList, days: Int, day: Int): SealList = {
        if (day == days) {
            Std.printString("");
            Std.printString("Simulation has ended!");
            Std.printString("");
            Std.printString("We've wound up with " ++ Std.intToString(length(pop)) ++ " seals!");
            pop
        } else {
            val oldLength: Int = length(pop);
            log("");
            log("On day " ++ Std.intToString(day) ++ ", there were "
                ++ Std.intToString(oldLength) ++ " seals...");
            val newPop: SealList = simulatei(pop, Nil());
            val newLength: Int = length(newPop);
            log("");
            if (newLength - oldLength <= 0) {
                log("No new seals were born today. :(")
            } else {
                log(Std.intToString(newLength - oldLength) ++ " new seals were born today.")
            };
            log("");
            printList(newPop)(sealInfo);

            if (length(newPop) == 0) {
                Std.printString("");
                Std.printString("Oh no! All the seals have died after " ++ Std.intToString(day) ++ " days. :'(");
                newPop
            } else {
                simulate(newPop, days, day + 1)
            }
        }
    }

    def killOrStep(s: Seal, sl: SealList): SealList = {
        if (dies(s)) {
            s match {
                case Sealio(n, a, g, m, c) =>
                    log("Oh no! " ++ n ++ " died at the young age of " ++ Std.intToString(a))
            };
            sl
        } else {
            Cons(step(s), sl) // It survived!
        }
    }

    def dies(s: Seal): Boolean = {
        s match {
            case Sealio(n, a, g, m, c) =>
                if (a < 4) { false }
                else {
                    Random.oneIn(10 - a) // The older you are, the higher chance of dying.
                }
        }
    }
    def step(s: Seal): Seal = {
        s match {
            case Sealio(n, a, g, m, c) =>
                Sealio(n, a+1, g, m, c)
        }
    }

    def runi(): Unit = {
        Std.printString("How many days would you like to simulate?");
        val days: Int = Std.readInt();
        if (days < 0) {
            Std.printString("Sorry, you can't simulate the past.");
            runi()
        } else {
            Std.printString("Wonderful! We'll simulate " ++ Std.intToString(days) ++ " days!");

            val init: SealList =
                Cons(Sealio("Adam", 1, Male(), Happy(), grey(40)),
                    Cons(Sealio("Eve", 1, Female(), Happy(), grey(150)),
                        Nil()
                    ));
            printList(init)(sealInfo);
            val res: SealList = simulate(init, days, 0);
            interestingStats(res);
            ()
        }
    }

    def run(start: Boolean): Unit = {
        if (start) {
            Std.printString("======= Welcome to the Seal Simulator =======");
            runi();
            run(false)
        } else {
            Std.printString("");
            Std.printString("Would you like to go again? (y/n)");
            val reply: String = Std.readString();
            if (Str.equals(reply, "yes") || Str.equals(reply, "y")) {
                runi();
                run(false)
            } else {
                Std.printString("Bye then! :)")
            }
        }
    }

    abstract class SealStat
    case class Stat(red: Int, blue: Int, green: Int, light: Int, dark: Int, splot: Int, prof: Int) extends SealStat

    def interestingStats(sl: SealList): Unit = {
        val stats: SealStat = interestingStatsi(sl, Stat(0, 0, 0, 0, 0, 0, 0));
        stats match {
            case Stat(red, blue, green, light, dark, splot, prof) =>
                if (0 < red) { Std.printString("Wow, there were " ++ Std.intToString(red) ++ " red seals!") } else { () };
                if (0 < green) { Std.printString("Wow, there were " ++ Std.intToString(green) ++ " green seals!") } else { () };
                if (0 < blue) { Std.printString("Wow, there were " ++ Std.intToString(blue) ++ " blue seals!") } else { () };
                if (0 < light) { Std.printString("Wow, there were " ++ Std.intToString(light) ++ " light seals!") } else { () };
                if (0 < dark) { Std.printString("Wow, there were " ++ Std.intToString(dark) ++ " dark seals!") } else { () };
                if (0 < splot) { Std.printString("Wow, there were " ++ Std.intToString(splot) ++ " splotchy seals!") } else { () };
                if (0 < prof) { Std.printString("Wow, a wisened Prof. Lionel showed up to teach the seals compiler design and survived!") } else { () }
        }
    }

    def interestingStatsi(sl: SealList, st: SealStat): SealStat = {
        st match {
            case Stat(red, blue, green, light, dark, splot, prof) =>
                sl match {
                    case Nil() => st
                    case Cons(Sealio(n, a, g, m, c), xs) =>
                        val cstr: String = colourString(c);
                        val red2: Int = if (Str.equals(cstr, "red")) { red + 1 } else { red };
                        val green2: Int = if (Str.equals(cstr, "green")) { green + 1 } else { green };
                        val blue2: Int = if (Str.equals(cstr, "blue")) { blue + 1 } else { blue };
                        val light2: Int = if (Str.equals(cstr, "lightish")) { light + 1 } else { red };
                        val dark2: Int = if (Str.equals(cstr, "darkish")) { dark + 1 } else { dark };
                        val splot2: Int = if (Str.equals(cstr, "splotchy")) { splot + 1 } else { splot };
                        val prof2: Int = if (Str.equals(n, "Prof. Lionel Parreux") && 7 < a) { prof + 1 } else { prof };
                        interestingStatsi(xs, Stat(red2, green2, blue2, light2, dark2, splot2, prof2))
                }
        }
    }

    def isGenderEqual(g1: Gender, g2: Gender): Boolean = {
        (g1, g2) match {
            case (Male(), Male()) => true
            case (Female(), Female()) => true
            case _ => false
        }
    }

    def sealInfo(s: Seal): String = {
        s match {
            case Sealio(n, a, g, m, c) =>
                val gendStr: String = g match { case Male() => "M" case Female() => "F" };
                n ++ " (" ++ gendStr ++ ", age " ++ Std.intToString(a) ++ ", " ++ colourString(c) ++ ") is feeling " ++ moodString(m) ++ " today."
        }
    }

    def colourString(c: Colour): String = {
        c match {
            case RGB(r, g, b) =>
                if (g < r/2 && b < r/2) { "red" } else {
                if (r < g/2 && b < g/2) { "green" } else {
                if (r < b/2 && g < b/2) { "blue" } else {
                if ((r - g < 10 || g - r < 10) && (r - b < 10 || b - r < 10) && (g - b < 10 || b - g < 10)) { "greyish" } else {
                if (r < 50 && g < 50 && b < 50) { "darkish" } else {
                if (200 < r && 200 < g && 200 < b) { "lightish" } else {
                    "splotchy"
                }
                }
                }
                }
                }
                }
        }
    }

    def moodString(m: Mood): String = {
        m match {
            case Happy() => "happy"
            case Belligerent() => "belligerent"
            case Crazy() => "crazy"
            case Moody() => "moody"
            case Sad() => "sad"
            case Angry() => "angry"
            case Bonkers() => "bonkers"
        }
    }

    def printList(sl: SealList): ((Seal => String) => Unit) = {
        \(printer: Seal => String) ->
            sl match {
                case Nil() => ()
                case Cons(x, xs) =>
                    log(printer(x));
                    printList(xs)(printer)
            }
    }

    def randomSeal(): (String, Gender) = {
        Random.int(20) match {
            case 0 => ("James", Male())
            case 1 => ("Mary", Female())
            case 2 => ("Tim", Male())
            case 3 => ("Rebecca", Female())
            case 4 => ("Joe", Male())
            case 5 => ("Zoe", Female())
            case 6 => ("Moses", Male())
            case 7 => ("Abigail", Female())
            case 8 => ("Alex", Male())
            case 9 => ("Sam", Female())
            case 10 => ("Samuel", Male())
            case 11 => ("Jean", Female())
            case 12 => ("Joseph", Male())
            case 13 => ("Tammy", Female())
            case 14 => ("Thomas the Tank Engine", Male())
            case 15 => ("Jill", Female())
            case 16 => ("Pikachu", Male())
            case 17 => ("Amelia Bedelia", Female())
            case 18 => ("Prof. Lionel Parreux", Male())
            case 19 => ("Matilda", Female())
        }
    }

    def randomMood(): Mood = {
        Random.int(7) match {
            case 0 => Happy()
            case 1 => Belligerent()
            case 2 => Crazy()
            case 3 => Moody()
            case 4 => Sad()
            case 5 => Angry()
            case 6 => Bonkers()
        }
    }

    def clamp(x: Int, lo: Int, hi: Int): Int = {
        if (x < lo) {
            lo
        } else {
            if (hi < x) {
                hi
            } else {
                x
            }
        }
    }

    def mergeAndVary(a: Int, b: Int, mutation: Int): Int = {
        val x: Int = (a + b) / 2 + (Random.range(-mutation, mutation)); // Allow some mutation.
        clamp(x, 0, 255)
    }

    def mixColour(a: Colour, b: Colour, mutation: Int): Colour = {
        (a, b) match {
            case (RGB(ra, ga, ba), RGB(rb, gb, bb)) =>
                RGB(
                    mergeAndVary(ra, rb, mutation),
                    mergeAndVary(ga, gb, mutation),
                    mergeAndVary(ba, bb, mutation)
                )
        }
    }

    def log(s: String): Unit = {
        Std.printString(" > " ++ s)
    }


    run(true)
}