#-*-coding: utf-8 -*-

__author__ = 'Longphi Nguyen'
# Reference:
# Marcel Caraciolo http://aimotion.blogspot.com/2012/08/introduction-to-recommendations-with.html

# This code takes input of form 'string, number, number',
# e.g. user, itemID, rating

# (The reference does not do this)
# 0. Preliminary data cleaning: Concacatenate (user, itemID, ratings)
# That is, if the data has multiple ratings for an itemID, then average the ratings.
# Mapper 0: [key, value]=[user, item, rating]
# Reducer 0: [key, value]=[user, (item, average rating)]

# 1. List the ratings a user gives as follows:
# Mapper 1: [key, value]=[user, (item, rating)]
# Reducer 1: [key, value]=[user, (item, rating) pairs]

# 2. Calculate the correlations between 2 items, only using users that
# have rated both items. Only need to calculate for itemID.1 < itemID.2,
# since correlation is symmetric. The reference does not use this symmetry.
# Reducer 2: [key, value]=[(item pairs), (rating pairs)]
# Mapper 2: [key, value]=[(item pairs), correlation]

from mrjob.job import MRJob
#from metrics import correlation
#from metrics import cosine, regularized_correlation
from math import sqrt

try:
    from itertools import combinations
except ImportError:
    from metrics import combinations


PRIOR_COUNT = 10
PRIOR_CORRELATION = 0


class SemicolonValueProtocol(object):

    # don't need to implement read() since we aren't using it

    def write(self, key, values):
        return key, values#.join(str(v) for v in values))


class MoviesSimilarities(MRJob):

    OUTPUT_PROTOCOL = SemicolonValueProtocol

    def steps(self):
        return [
            self.mr(mapper=self.mapper1, reducer=self.reducer1),
            self.mr(mapper=self.mapper2, reducer=self.reducer2)]

    def mapper1(self, key, line):
        user, item, rating = line.split('|')
        yield user, (item, float(rating))

    def reducer1(self, user, pairs):
#        ratingSum = 0
#        numItems = 0
        pairsList = []

        for item, rating in pairs:
#            ratingSum+=rating
#            numItems+=1
            pairsList.append((item, rating))

        yield user, (pairsList)#(numItems, ratingSum, pairsList)

    def mapper2(self, user, pairs):
        for pair1, pair2 in combinations(pairs, 2):
            #if pair1[0]<pair2[0]:
            yield (pair1[0], pair2[0]), (pair1[1], pair2[1])
            #else:
            #    yield (pair2[0], pair1[0]), (pair2[1], pair1[1])

    def reducer2(self, item_pair, rating_pair):
        sum1, sum2, sum1_sq, sum2_sq, sum12 = 0.0, 0.0, 0.0, 0.0, 0.0
        n=0

        for rating1, rating2 in rating_pair:
            sum1 += rating1
            sum2 += rating2
            sum1_sq += rating1*rating1
            sum2_sq += rating2*rating2
            sum12 += rating1*rating2
            n+=1

        # Calculate correlation, using the form without means to avoid having to
        # calculate the means via a map/reduce before this one.
        denominator = sqrt(n*sum1_sq - sum1*sum1) * sqrt(n*sum2_sq - sum2*sum2)
        if denominator:
            correlation = (n*sum12 - sum1*sum2)/denominator
        else: # The denominator isn't valid, so set correlation=0
            correlation = 0.0

        yield item_pair, correlation


############
    def calculate_ranking(self, item_keys, values):
        corr_sim, cos_sim, reg_corr_sim, jaccard_sim, n = values
        item_x, item_y = item_keys
        if int(n) > 0:
            yield (item_x, corr_sim, cos_sim, reg_corr_sim, jaccard_sim), \
                     (item_y, n)

    def top_similar_items(self, key_sim, similar_ns):
        item_x, corr_sim, cos_sim, reg_corr_sim, jaccard_sim = key_sim
        for item_y, n in similar_ns:
            yield None, (item_x, item_y, corr_sim, cos_sim, reg_corr_sim,
                         jaccard_sim, n)


if __name__ == '__main__':
    MoviesSimilarities.run()
