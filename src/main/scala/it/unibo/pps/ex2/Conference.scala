package it.unibo.pps.ex2

import it.unibo.pps.ex2

object Conference:
  /** Article Type. */
  opaque type Article = Int

  object Article:
    def apply(id: Int): Article =
      require(id >= 0)
      id

  /** Score Type. */
  opaque type Score = Double

  object Score:
    def apply(score: Int): Score =
      require(score >= 0 && score <= 10)
      score

    /** Score = 0. */
    def zero: Score = apply(0)

    /** Score = 10. */
    def max: Score = apply(10)

  /** A set of question that the reviewer has to reply to review an article. */
  enum Question:
    /** Is it important for the conference? */
    case RELEVANCE
    /** Does it produce scientific contribute? */
    case SIGNIFICANCE
    /** Do you feel competent on commenting it? */
    case CONFIDENCE
    /** Is the article to be accepted? */
    case FINAL

  /**
   * An interface modeling the results of reviewing articles of a conference.
   * Each reviewer reads an article, and answers to a number of questions
   * with a score from 0 (bad) to 10 (excellent).
   * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the
   * system does not keep track of the identity of reviewers.
   */
  trait ConferenceReviewing:
    /**
     * Load a review for the specified article, with complete scores as a map.
     * */
    def loadReview(article: Article)(scores: Map[Question, Score]): Unit

    /**
     * @return the scores given to the specified article and specified question,
     *         as an (ascending-ordered) list. */
    def orderedScores(article: Article, question: Question): Seq[Score]

    /**
     * @return the average score to question FINAL taken by the specified article.
     * */
    def averageFinalScore(article: Article): Score

    /**
     * An article is considered accepted if its averageFinalScore (not weighted) is > 5,
     * and at least one RELEVANCE score that is >= 8.
     *
     * @return the set of accepted articles
     * */
    def acceptedArticles: Set[Article]

    /**
     * @return accepted articles as a list of pairs article+averageFinalScore,
     *         ordered from worst to best based on averageFinalScore.
     */
    def sortedAcceptedArticles: Seq[(Article, Score)]

    /**
     * @return a map from articles to their average "weighted final score",
     *         namely, the average value of CONFIDENCE*FINAL/10.
     */
    def averageWeightedFinalScoreMap: Map[Article, Score]
  end ConferenceReviewing

  object ConferenceReviewing:
    def apply(): ConferenceReviewing = new ConferenceReviewing:
      private val thresholdFinal = Score(5)
      private val thresholdRelevance = Score(8)
      private var reviews: Seq[(Article, Map[Question, Score])] = Seq.empty

      private def scores(article: Article, question: Question): Seq[Score] =
        reviews filter (_._1 == article) flatMap (_._2.get(question))

      private def articles: Set[Article] = reviews.map((a, _) => a).toSet

      extension (scores: Seq[Score])
        private def averageScore: Score =
          if scores.isEmpty then Score.zero else scores.sum / scores.length

      private def averageWeightedFinalScore(article: Article): Score =
        val articleScores = scores.curried(article)
        (articleScores(Question.FINAL) zip articleScores(Question.CONFIDENCE))
          .map(_ * _ / Score.max)
          .averageScore

      override def loadReview(article: Article)(scores: Map[Question, Score]): Unit =
        require(scores.keySet == Question.values.toSet)
        reviews +:= article -> scores

      override def orderedScores(article: Article, question: Question): Seq[Score] =
        scores(article, question).sorted

      override def averageFinalScore(article: Article): Score =
        scores(article, Question.FINAL).averageScore

      override def acceptedArticles: Set[Article] = articles
        .filter(averageFinalScore(_) > thresholdFinal)
        .filter(scores(_, Question.RELEVANCE) exists (_ >= thresholdRelevance))

      override def sortedAcceptedArticles: List[(Article, Score)] = acceptedArticles
        .map(a => (a, averageFinalScore(a)))
        .toList
        .sortBy((_, s) => s)

      override def averageWeightedFinalScoreMap: Map[Article, Score] =
        articles.map(a => a -> averageWeightedFinalScore(a)).toMap

  end ConferenceReviewing

end Conference


@main def testArticle(): Unit =
  import Conference.*
  // val negative = Article(-1) // IllegalArgumentException
  val a1: Article /* Int */ = Article(1)
  println(a1)

@main def testScore(): Unit =
  import Conference.*
  // val negative = Score(-5) // IllegalArgumentException
  // val upToTen = Score(11) // IllegalArgumentException
  // val decimal = Score(7.5) // Score requires an Int!
  val s: Score /* Double */ = Score(10)
  println(s)

@main def testConference(): Unit =
  import Conference.*
  import Question.*
  val conferenceReviewing = ConferenceReviewing()
  val a1 = Article(1)
  val a2 = Article(2)
  // Load Article 1
  // first reviewer
  conferenceReviewing.loadReview(a1):
    Map(
      RELEVANCE -> Score(7),
      SIGNIFICANCE -> Score(8),
      CONFIDENCE -> Score(7),
      FINAL -> Score(8))
  // second reviewer
  conferenceReviewing.loadReview(a1):
    Map(
      RELEVANCE -> Score(6),
      SIGNIFICANCE -> Score(7),
      CONFIDENCE -> Score(6),
      FINAL -> Score(7))
  // Load Article 2
  // first reviewer
  conferenceReviewing.loadReview(a2):
    Map(
      RELEVANCE -> Score(9),
      SIGNIFICANCE -> Score(9),
      CONFIDENCE -> Score(8),
      FINAL -> Score(9))
  // Ordered scores for RELEVANCE in Article 1
  println(conferenceReviewing.orderedScores(a1, RELEVANCE))
  // Accepted articles
  println(conferenceReviewing.acceptedArticles)
  // Accepted articles sorted by score
  println(conferenceReviewing.sortedAcceptedArticles)
  // Article 1's average final score
  println(conferenceReviewing.averageFinalScore(a1))
  // Article 2's average final score
  println(conferenceReviewing.averageFinalScore(a2))
  // Weighted final score per Article
  println(conferenceReviewing.averageWeightedFinalScoreMap)
